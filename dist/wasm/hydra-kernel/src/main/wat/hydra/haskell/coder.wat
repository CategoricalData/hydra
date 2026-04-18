(module
  (import "hydra.analysis" "hydra.analysis.module_contains_binary_literals" (func $hydra.analysis.module_contains_binary_literals (param i32) (result i32) ) )
  (import "hydra.analysis" "hydra.analysis.module_contains_decimal_literals" (func $hydra.analysis.module_contains_decimal_literals (param i32) (result i32) ) )
  (import "hydra.annotations" "hydra.annotations.get_term_description" (func $hydra.annotations.get_term_description (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.annotations" "hydra.annotations.get_type_description" (func $hydra.annotations.get_type_description (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.dependencies" "hydra.dependencies.simplify_term" (func $hydra.dependencies.simplify_term (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.type" (func $hydra.encode.core.type (param i32) (result i32) ) )
  (import "hydra.formatting" "hydra.formatting.capitalize" (func $hydra.formatting.capitalize (param i32) (result i32) ) )
  (import "hydra.formatting" "hydra.formatting.decapitalize" (func $hydra.formatting.decapitalize (param i32) (result i32) ) )
  (import "hydra.haskell.serde" "hydra.haskell.serde.module_to_expr" (func $hydra.haskell.serde.module_to_expr (param i32) (result i32) ) )
  (import "hydra.haskell.utils" "hydra.haskell.utils.application_pattern" (func $hydra.haskell.utils.application_pattern (param i32) (param i32) (result i32) ) )
  (import "hydra.haskell.utils" "hydra.haskell.utils.element_reference" (func $hydra.haskell.utils.element_reference (param i32) (param i32) (result i32) ) )
  (import "hydra.haskell.utils" "hydra.haskell.utils.hsapp" (func $hydra.haskell.utils.hsapp (param i32) (param i32) (result i32) ) )
  (import "hydra.haskell.utils" "hydra.haskell.utils.hslambda" (func $hydra.haskell.utils.hslambda (param i32) (param i32) (result i32) ) )
  (import "hydra.haskell.utils" "hydra.haskell.utils.hslit" (func $hydra.haskell.utils.hslit (param i32) (result i32) ) )
  (import "hydra.haskell.utils" "hydra.haskell.utils.hsvar" (func $hydra.haskell.utils.hsvar (param i32) (result i32) ) )
  (import "hydra.haskell.utils" "hydra.haskell.utils.namespaces_for_module" (func $hydra.haskell.utils.namespaces_for_module (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.haskell.utils" "hydra.haskell.utils.newtype_accessor_name" (func $hydra.haskell.utils.newtype_accessor_name (param i32) (result i32) ) )
  (import "hydra.haskell.utils" "hydra.haskell.utils.raw_name" (func $hydra.haskell.utils.raw_name (param i32) (result i32) ) )
  (import "hydra.haskell.utils" "hydra.haskell.utils.record_field_reference" (func $hydra.haskell.utils.record_field_reference (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.haskell.utils" "hydra.haskell.utils.simple_name" (func $hydra.haskell.utils.simple_name (param i32) (result i32) ) )
  (import "hydra.haskell.utils" "hydra.haskell.utils.simple_value_binding" (func $hydra.haskell.utils.simple_value_binding (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.haskell.utils" "hydra.haskell.utils.to_type_application" (func $hydra.haskell.utils.to_type_application (param i32) (result i32) ) )
  (import "hydra.haskell.utils" "hydra.haskell.utils.union_field_reference" (func $hydra.haskell.utils.union_field_reference (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.haskell.utils" "hydra.haskell.utils.unpack_forall_type" (func $hydra.haskell.utils.unpack_forall_type (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.fields_of" (func $hydra.lexical.fields_of (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.concat2" (func $hydra.lib.lists.concat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.cons" (func $hydra.lib.lists.cons (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.foldl" (func $hydra.lib.lists.foldl (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.maybe_head" (func $hydra.lib.lists.maybe_head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.reverse" (func $hydra.lib.lists.reverse (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.uncons" (func $hydra.lib.lists.uncons (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip_with" (func $hydra.lib.lists.zip_with (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigfloat_to_float64" (func $hydra.lib.literals.bigfloat_to_float64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.binary_to_string" (func $hydra.lib.literals.binary_to_string (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.int16_to_bigint" (func $hydra.lib.literals.int16_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.int64_to_bigint" (func $hydra.lib.literals.int64_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.int8_to_bigint" (func $hydra.lib.literals.int8_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_decimal" (func $hydra.lib.literals.show_decimal (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.uint16_to_bigint" (func $hydra.lib.literals.uint16_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.uint32_to_bigint" (func $hydra.lib.literals.uint32_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.uint64_to_bigint" (func $hydra.lib.literals.uint64_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.uint8_to_bigint" (func $hydra.lib.literals.uint8_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.or" (func $hydra.lib.logic.or (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.keys" (func $hydra.lib.maps.keys (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.map" (func $hydra.lib.maps.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.null" (func $hydra.lib.maps.null (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.singleton" (func $hydra.lib.maps.singleton (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.union" (func $hydra.lib.maps.union (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.add" (func $hydra.lib.math.add (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.bind" (func $hydra.lib.maybes.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cases" (func $hydra.lib.maybes.cases (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.from_maybe" (func $hydra.lib.maybes.from_maybe (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.is_just" (func $hydra.lib.maybes.is_just (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.is_nothing" (func $hydra.lib.maybes.is_nothing (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.insert" (func $hydra.lib.sets.insert (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.member" (func $hydra.lib.sets.member (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.null" (func $hydra.lib.sets.null (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.union" (func $hydra.lib.sets.union (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.split_on" (func $hydra.lib.strings.split_on (param i32) (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.local_name_of" (func $hydra.names.local_name_of (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.namespace_of" (func $hydra.names.namespace_of (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.namespace_to_file_path" (func $hydra.names.namespace_to_file_path (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.qname" (func $hydra.names.qname (param i32) (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.qualify_name" (func $hydra.names.qualify_name (param i32) (result i32) ) )
  (import "hydra.names" "hydra.names.unqualify_name" (func $hydra.names.unqualify_name (param i32) (result i32) ) )
  (import "hydra.predicates" "hydra.predicates.is_serializable_by_name" (func $hydra.predicates.is_serializable_by_name (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.require_union_field" (func $hydra.resolution.require_union_field (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.resolution" "hydra.resolution.require_union_type" (func $hydra.resolution.require_union_type (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.fold_over_term" (func $hydra.rewriting.fold_over_term (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.fold_over_type" (func $hydra.rewriting.fold_over_type (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.rewriting" "hydra.rewriting.rewrite_term" (func $hydra.rewriting.rewrite_term (param i32) (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.parenthesize" (func $hydra.serialization.parenthesize (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.print_expr" (func $hydra.serialization.print_expr (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_term" (func $hydra.strip.deannotate_term (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (import "hydra.variables" "hydra.variables.is_free_variable_in_term" (func $hydra.variables.is_free_variable_in_term (param i32) (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\00\00\00\00\01\00\00\00\2e\01\00\00\00\42\0c\00\00\00\42\2e\42\79\74\65\53\74\72\69\6e\67\04\00\00\00\42\6f\6f\6c\0f\00\00\00\44\61\74\61\2e\42\79\74\65\53\74\72\69\6e\67\08\00\00\00\44\61\74\61\2e\49\6e\74\08\00\00\00\44\61\74\61\2e\4d\61\70\0f\00\00\00\44\61\74\61\2e\53\63\69\65\6e\74\69\66\69\63\08\00\00\00\44\61\74\61\2e\53\65\74\06\00\00\00\44\6f\75\62\6c\65\06\00\00\00\45\69\74\68\65\72\04\00\00\00\45\6e\75\6d\02\00\00\00\45\71\05\00\00\00\46\61\6c\73\65\05\00\00\00\46\6c\6f\61\74\12\00\00\00\48\79\64\72\61\2e\4c\69\62\2e\4c\69\74\65\72\61\6c\73\01\00\00\00\49\07\00\00\00\49\2e\49\6e\74\31\36\07\00\00\00\49\2e\49\6e\74\36\34\06\00\00\00\49\2e\49\6e\74\38\03\00\00\00\49\6e\74\07\00\00\00\49\6e\74\65\67\65\72\04\00\00\00\4a\75\73\74\04\00\00\00\4c\65\66\74\08\00\00\00\4c\69\74\65\72\61\6c\73\17\00\00\00\4c\69\74\65\72\61\6c\73\2e\73\74\72\69\6e\67\54\6f\42\69\6e\61\72\79\18\00\00\00\4c\69\74\65\72\61\6c\73\2e\73\74\72\69\6e\67\54\6f\44\65\63\69\6d\61\6c\01\00\00\00\4d\05\00\00\00\4d\2e\4d\61\70\07\00\00\00\4d\2e\65\6d\70\74\79\0a\00\00\00\4d\2e\66\72\6f\6d\4c\69\73\74\05\00\00\00\4d\61\79\62\65\07\00\00\00\4e\6f\74\68\69\6e\67\03\00\00\00\4f\72\64\08\00\00\00\4f\72\64\65\72\69\6e\67\07\00\00\00\50\72\65\6c\75\64\65\04\00\00\00\52\65\61\64\05\00\00\00\52\69\67\68\74\01\00\00\00\53\05\00\00\00\53\2e\53\65\74\07\00\00\00\53\2e\65\6d\70\74\79\0a\00\00\00\53\2e\66\72\6f\6d\4c\69\73\74\03\00\00\00\53\63\69\0e\00\00\00\53\63\69\2e\53\63\69\65\6e\74\69\66\69\63\04\00\00\00\53\68\6f\77\06\00\00\00\53\74\72\69\6e\67\04\00\00\00\54\72\75\65\04\00\00\00\56\6f\69\64\01\00\00\00\5f\06\00\00\00\5f\74\79\70\65\5f\0b\00\00\00\64\65\63\6f\64\65\46\6c\6f\61\74\0b\00\00\00\65\6e\63\6f\64\65\46\6c\6f\61\74\08\00\00\00\65\71\75\61\6c\69\74\79\04\00\00\00\66\61\69\6c\0a\00\00\00\68\61\73\6b\65\6c\6c\56\61\72\02\00\00\00\68\73\0f\00\00\00\68\79\64\72\61\2e\63\6f\72\65\2e\4e\61\6d\65\0f\00\00\00\68\79\64\72\61\2e\63\6f\72\65\2e\54\79\70\65\03\00\00\00\6d\61\70\08\00\00\00\6f\72\64\65\72\69\6e\67\0b\00\00\00\70\6c\61\63\65\68\6f\6c\64\65\72\04\00\00\00\70\75\72\65\06\00\00\00\72\65\63\6f\72\64\03\00\00\00\73\75\6d\16\00\00\00\73\75\70\70\6f\72\74\65\64\20\69\6e\74\65\67\65\72\20\74\79\70\65\11\00\00\00\73\75\70\70\6f\72\74\65\64\20\6c\69\74\65\72\61\6c\16\00\00\00\73\75\70\70\6f\72\74\65\64\20\6c\69\74\65\72\61\6c\20\74\79\70\65\0e\00\00\00\73\75\70\70\6f\72\74\65\64\20\74\65\72\6d\0e\00\00\00\73\75\70\70\6f\72\74\65\64\20\74\79\70\65\01\00\00\00\76\08\00\00\00\76\61\72\69\61\62\6c\65\01\00\00\00\78")
  (global $__bump_ptr (mut i32) i32.const 1872 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.haskell.coder.adapt_type_to_haskell_and_encode" (func $hydra.haskell.coder.adapt_type_to_haskell_and_encode) )
  (export "hydra.haskell.coder.constant_for_field_name" (func $hydra.haskell.coder.constant_for_field_name) )
  (export "hydra.haskell.coder.constant_for_type_name" (func $hydra.haskell.coder.constant_for_type_name) )
  (export "hydra.haskell.coder.construct_module" (func $hydra.haskell.coder.construct_module) )
  (export "hydra.haskell.coder.empty_metadata" (func $hydra.haskell.coder.empty_metadata) )
  (export "hydra.haskell.coder.encode_case_expression" (func $hydra.haskell.coder.encode_case_expression) )
  (export "hydra.haskell.coder.encode_lambda_term" (func $hydra.haskell.coder.encode_lambda_term) )
  (export "hydra.haskell.coder.encode_literal" (func $hydra.haskell.coder.encode_literal) )
  (export "hydra.haskell.coder.encode_projection" (func $hydra.haskell.coder.encode_projection) )
  (export "hydra.haskell.coder.encode_standalone_cases" (func $hydra.haskell.coder.encode_standalone_cases) )
  (export "hydra.haskell.coder.encode_term" (func $hydra.haskell.coder.encode_term) )
  (export "hydra.haskell.coder.encode_type" (func $hydra.haskell.coder.encode_type) )
  (export "hydra.haskell.coder.encode_type_with_class_assertions" (func $hydra.haskell.coder.encode_type_with_class_assertions) )
  (export "hydra.haskell.coder.encode_unwrap" (func $hydra.haskell.coder.encode_unwrap) )
  (export "hydra.haskell.coder.extend_meta_for_term" (func $hydra.haskell.coder.extend_meta_for_term) )
  (export "hydra.haskell.coder.extend_meta_for_type" (func $hydra.haskell.coder.extend_meta_for_type) )
  (export "hydra.haskell.coder.find_ord_variables" (func $hydra.haskell.coder.find_ord_variables) )
  (export "hydra.haskell.coder.gather_metadata" (func $hydra.haskell.coder.gather_metadata) )
  (export "hydra.haskell.coder.get_implicit_type_classes" (func $hydra.haskell.coder.get_implicit_type_classes) )
  (export "hydra.haskell.coder.include_type_definitions" (func $hydra.haskell.coder.include_type_definitions) )
  (export "hydra.haskell.coder.key_haskell_var" (func $hydra.haskell.coder.key_haskell_var) )
  (export "hydra.haskell.coder.module_to_haskell" (func $hydra.haskell.coder.module_to_haskell) )
  (export "hydra.haskell.coder.module_to_haskell_module" (func $hydra.haskell.coder.module_to_haskell_module) )
  (export "hydra.haskell.coder.name_decls" (func $hydra.haskell.coder.name_decls) )
  (export "hydra.haskell.coder.set_meta_uses_byte_string" (func $hydra.haskell.coder.set_meta_uses_byte_string) )
  (export "hydra.haskell.coder.set_meta_uses_int" (func $hydra.haskell.coder.set_meta_uses_int) )
  (export "hydra.haskell.coder.set_meta_uses_map" (func $hydra.haskell.coder.set_meta_uses_map) )
  (export "hydra.haskell.coder.set_meta_uses_set" (func $hydra.haskell.coder.set_meta_uses_set) )
  (export "hydra.haskell.coder.to_data_declaration" (func $hydra.haskell.coder.to_data_declaration) )
  (export "hydra.haskell.coder.to_type_declarations_from" (func $hydra.haskell.coder.to_type_declarations_from) )
  (export "hydra.haskell.coder.type_decl" (func $hydra.haskell.coder.type_decl) )
  (export "hydra.haskell.coder.type_scheme_constraints_to_class_map" (func $hydra.haskell.coder.type_scheme_constraints_to_class_map) )
  (export "hydra.haskell.coder.use_core_import" (func $hydra.haskell.coder.use_core_import) )
  (func $hydra.haskell.coder.adapt_type_to_haskell_and_encode (param $namespaces i32) (param $typ i32) (param $cx i32) (param $g i32) (result i32)
  (local $__rec_ptr i32)
  (local $enc i32)
  (local $t i32)
  (local $v i32)
  local.get $namespaces
  local.get $t
  local.get $cx
  local.get $g
  call $hydra.haskell.coder.encode_type
  local.set $enc
  (block $end_type (result i32)
  (block $variable
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $variable $variable
)
  local.get $v
  drop
  local.get $typ
  drop
  local.get $enc
  drop
  i32.const 0
  br $end_type
)
)
  (func $hydra.haskell.coder.constant_for_field_name (param $tname i32) (param $fname i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1552
  local.get $tname
  call $hydra.names.local_name_of
  i32.const 1552
  local.get $fname
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
)
  (func $hydra.haskell.coder.constant_for_type_name (param $tname i32) (result i32)
  i32.const 1552
  local.get $tname
  call $hydra.names.local_name_of
  call $hydra.lib.strings.cat2
)
  (func $hydra.haskell.coder.construct_module (param $namespaces i32) (param $mod i32) (param $defs i32) (param $cx i32) (param $g i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  (local $type i32)
  (local $v i32)
  (local $x i32)
  local.get $namespace
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $h
  (block $end_definition (result i32)
  (block $term
  (block $type
  local.get $def
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $type $term $term
)
  local.get $v
  drop
  local.get $type
  i32.load
  local.set $name
  local.get $type
  i32.load offset=4
  i32.load offset=4
  local.set $typ
  local.get $namespaces
  local.get $name
  local.get $typ
  local.get $cx
  local.get $g
  call $hydra.haskell.coder.to_type_declarations_from
  br $end_definition
)
  local.get $v
  drop
  local.get $namespaces
  local.get $term
  local.get $cx
  local.get $g
  call $hydra.haskell.coder.to_data_declaration
  i32.const 1
  local.get $d
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
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
  call $hydra.lib.eithers.bind
  br $end_definition
)
  local.set $create_declarations
  i32.const 1028
  i32.const 0
  i32.const 1028
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
  drop
  local.get $h
  drop
  i32.const 0
  local.set $name
  i32.const 1
  local.get $name
  drop
  local.get $import_name
  drop
  i32.const 0
  local.get $alias
  i32.const 0
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
  local.set $to_import
  local.get $to_import
  local.get $namespaces
  i32.load offset=4
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
  local.set $domain_imports
  local.get $defs
  call $hydra.haskell.coder.gather_metadata
  local.set $meta
  local.get $flag
  local.get $triple
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
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
  i32.const 1
  i32.const 0
  local.get $n
  call $hydra.haskell.utils.simple_name
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
  local.get $hidden
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
  call $hydra.lib.logic.if_else
  local.set $spec
  local.get $malias
  call $hydra.lib.maybes.is_just
  local.get $name
  local.get $x
  local.get $malias
  call $hydra.lib.maybes.map
  local.get $spec
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
  local.set $to_import
  local.get $to_import
  i32.const 1426
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
  i32.const 1156
  i32.const 1414
  i32.const 1567
  i32.const 1582
  i32.const 1609
  i32.const 1675
  i32.const 1709
  i32.const 1727
  i32.const 36
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 8
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=32
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=28
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
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  i32.const 1105
  i32.const 1493
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
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
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  local.get $meta
  i32.load
  drop
  i32.const 1062
  i32.const 1033
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
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
  drop
  local.get $cond_import
  drop
  i32.const 0
  local.get $meta
  i32.load offset=4
  drop
  i32.const 1081
  i32.const 1210
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
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
  drop
  local.get $cond_import
  drop
  i32.const 0
  local.get $meta
  i32.load offset=8
  drop
  i32.const 1093
  i32.const 1348
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
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
  drop
  local.get $cond_import
  drop
  i32.const 0
  local.get $meta
  i32.load offset=12
  drop
  i32.const 1124
  i32.const 1454
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
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
  drop
  local.get $cond_import
  drop
  i32.const 0
  local.get $mod
  call $hydra.analysis.module_contains_binary_literals
  local.get $mod
  call $hydra.analysis.module_contains_decimal_literals
  call $hydra.lib.logic.or
  i32.const 1188
  i32.const 1281
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
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
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  call $hydra.lib.logic.if_else
  i32.const 32
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 7
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=28
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
  call $hydra.lib.lists.concat
  call $hydra.lib.lists.map
  local.set $standard_imports
  local.get $create_declarations
  local.get $defs
  call $hydra.lib.eithers.map_list
  local.get $decl_lists
  call $hydra.lib.lists.concat
  local.set $decls
  local.get $mod
  i32.load offset=16
  local.set $mc
  i32.const 1
  local.get $mc
  local.get $mod
  i32.load
  drop
  local.get $h
  drop
  i32.const 0
  drop
  local.get $import_name
  drop
  i32.const 0
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
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
  local.get $imports
  local.get $decls
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
  call $hydra.lib.eithers.bind
)
  (func $hydra.haskell.coder.empty_metadata (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
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
)
  (func $hydra.haskell.coder.encode_case_expression (param $depth i32) (param $namespaces i32) (param $stmt i32) (param $scrutinee i32) (param $cx i32) (param $g i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $alt i32)
  (local $args i32)
  (local $cs i32)
  (local $d i32)
  (local $dcases i32)
  (local $def i32)
  (local $dn i32)
  (local $ecases i32)
  (local $f i32)
  (local $field i32)
  (local $field_map i32)
  (local $field_type i32)
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
  (local $v i32)
  (local $v0 i32)
  (local $v1 i32)
  (local $x i32)
  local.get $stmt
  i32.load
  local.set $dn
  local.get $stmt
  i32.load offset=4
  local.set $def
  local.get $stmt
  i32.load offset=8
  local.set $fields
  local.get $field
  i32.load
  local.set $fn
  local.get $field
  i32.load offset=4
  local.set $fun'
  i32.const 1843
  local.get $depth
  call $hydra.lib.literals.show_int32
  call $hydra.lib.strings.cat2
  local.set $v0
  i32.const 1
  local.get $fun'
  i32.const 19
  local.get $v0
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
  local.set $raw
  local.get $raw
  call $hydra.dependencies.simplify_term
  local.set $rhs_term
  local.get $v0
  local.get $rhs_term
  call $hydra.variables.is_free_variable_in_term
  i32.const 0
  local.get $v0
  call $hydra.lib.logic.if_else
  local.set $v1
  local.get $g
  i32.load
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  local.get $g
  i32.load offset=24
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  call $hydra.lib.sets.union
  local.get $namespaces
  local.get $dn
  local.get $fn
  call $hydra.haskell.utils.union_field_reference
  local.set $hname
  local.get $fn
  local.get $field_map
  call $hydra.lib.maps.lookup
  i32.const 0
  i32.const 7
  i32.const 2
  local.get $fn
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
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
  local.get $field_type
  i32.load offset=4
  local.set $ft
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.set $no_args
  i32.const 4
  local.get $v1
  call $hydra.haskell.utils.raw_name
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
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  local.set $single_arg
  (block $end_type (result i32)
  (block $unit
  local.get $ft
  call $hydra.strip.deannotate_type
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $unit $unit
)
  local.get $v
  drop
  i32.const 1
  local.get $no_args
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
  br $end_type
)
  call $hydra.lib.maybes.cases
  local.get $hname
  local.get $args
  call $hydra.haskell.utils.application_pattern
  local.set $lhs
  local.get $x
  local.get $depth
  i32.const 1
  call $hydra.lib.math.add
  local.get $namespaces
  local.get $rhs_term
  local.get $cx
  local.get $g
  call $hydra.haskell.coder.encode_term
  call $hydra.lib.eithers.map
  i32.const 1
  local.get $lhs
  local.get $rhs
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
  call $hydra.lib.eithers.bind
  local.set $to_alt
  local.get $cx
  local.get $g
  local.get $dn
  call $hydra.resolution.require_union_type
  local.get $f
  i32.load
  local.get $f
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
  local.set $to_field_map_entry
  local.get $to_field_map_entry
  local.get $rt
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
  local.set $field_map
  local.get $field_map
  drop
  local.get $to_alt
  drop
  i32.const 0
  local.get $fields
  call $hydra.lib.eithers.map_list
  local.get $def
  i32.const 1
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
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
  local.get $x
  local.get $depth
  local.get $namespaces
  local.get $d
  local.get $cx
  local.get $g
  call $hydra.haskell.coder.encode_term
  call $hydra.lib.eithers.map
  i32.const 4
  i32.const 0
  call $hydra.haskell.utils.raw_name
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
  local.set $lhs
  local.get $lhs
  local.get $cs
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
  local.set $alt
  i32.const 1
  local.get $alt
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.maybes.cases
  i32.const 1
  i32.const 1
  local.get $scrutinee
  local.get $ecases
  local.get $dcases
  call $hydra.lib.lists.concat2
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.haskell.coder.encode_lambda_term (param $depth i32) (param $namespaces i32) (param $lam i32) (param $cx i32) (param $g i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $body i32)
  (local $hbody i32)
  (local $v i32)
  local.get $lam
  i32.load
  local.set $v
  local.get $lam
  i32.load offset=8
  local.set $body
  local.get $depth
  local.get $namespaces
  local.get $body
  local.get $cx
  local.get $g
  call $hydra.haskell.coder.encode_term
  i32.const 1
  local.get $namespaces
  local.get $v
  call $hydra.haskell.utils.element_reference
  local.get $hbody
  call $hydra.haskell.utils.hslambda
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
  (func $hydra.haskell.coder.encode_literal (param $l i32) (param $cx i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $bs i32)
  (local $d i32)
  (local $f i32)
  (local $fv i32)
  (local $i i32)
  (local $iv i32)
  (local $s i32)
  (local $v i32)
  (block $end_literal (result i32)
  (block $string
  (block $integer
  (block $float
  (block $decimal
  (block $boolean
  (block $binary
  local.get $l
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $binary $boolean $decimal $float $integer $string $string
)
  local.get $v
  drop
  i32.const 1
  i32.const 1293
  call $hydra.haskell.utils.hsvar
  i32.const 5
  local.get $bs
  call $hydra.lib.literals.binary_to_string
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
  call $hydra.haskell.utils.hslit
  call $hydra.haskell.utils.hsapp
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
  br $end_literal
)
  local.get $v
  drop
  i32.const 1
  local.get $b
  i32.const 1536
  i32.const 1170
  call $hydra.lib.logic.if_else
  call $hydra.haskell.utils.hsvar
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
  br $end_literal
)
  local.get $v
  drop
  i32.const 1
  i32.const 1320
  call $hydra.haskell.utils.hsvar
  i32.const 5
  local.get $d
  call $hydra.lib.literals.show_decimal
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
  call $hydra.haskell.utils.hslit
  call $hydra.haskell.utils.hsapp
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
  br $end_literal
)
  local.get $v
  drop
  (block $end_float_value (result i32)
  (block $bigfloat
  (block $float64
  (block $float32
  local.get $fv
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $float32 $float64 $bigfloat $bigfloat
)
  local.get $v
  drop
  i32.const 1
  i32.const 2
  local.get $f
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
  call $hydra.haskell.utils.hslit
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
  br $end_float_value
)
  local.get $v
  drop
  i32.const 1
  i32.const 1
  local.get $f
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
  call $hydra.haskell.utils.hslit
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
  br $end_float_value
)
  local.get $v
  drop
  i32.const 1
  i32.const 1
  local.get $f
  call $hydra.lib.literals.bigfloat_to_float64
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
  call $hydra.haskell.utils.hslit
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
  br $end_float_value
)
  br $end_literal
)
  local.get $v
  drop
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
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $bigint $int8 $int16 $int32 $int64 $uint8 $uint16 $uint32 $uint64 $uint64
)
  local.get $v
  drop
  i32.const 1
  i32.const 4
  local.get $i
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
  call $hydra.haskell.utils.hslit
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
  br $end_integer_value
)
  local.get $v
  drop
  i32.const 1
  i32.const 4
  local.get $i
  call $hydra.lib.literals.int8_to_bigint
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
  call $hydra.haskell.utils.hslit
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
  br $end_integer_value
)
  local.get $v
  drop
  i32.const 1
  i32.const 4
  local.get $i
  call $hydra.lib.literals.int16_to_bigint
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
  call $hydra.haskell.utils.hslit
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
  br $end_integer_value
)
  local.get $v
  drop
  i32.const 1
  i32.const 3
  local.get $i
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
  call $hydra.haskell.utils.hslit
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
  br $end_integer_value
)
  local.get $v
  drop
  i32.const 1
  i32.const 4
  local.get $i
  call $hydra.lib.literals.int64_to_bigint
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
  call $hydra.haskell.utils.hslit
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
  br $end_integer_value
)
  local.get $v
  drop
  i32.const 1
  i32.const 4
  local.get $i
  call $hydra.lib.literals.uint8_to_bigint
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
  call $hydra.haskell.utils.hslit
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
  br $end_integer_value
)
  local.get $v
  drop
  i32.const 1
  i32.const 4
  local.get $i
  call $hydra.lib.literals.uint16_to_bigint
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
  call $hydra.haskell.utils.hslit
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
  br $end_integer_value
)
  local.get $v
  drop
  i32.const 1
  i32.const 4
  local.get $i
  call $hydra.lib.literals.uint32_to_bigint
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
  call $hydra.haskell.utils.hslit
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
  br $end_integer_value
)
  local.get $v
  drop
  i32.const 1
  i32.const 4
  local.get $i
  call $hydra.lib.literals.uint64_to_bigint
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
  call $hydra.haskell.utils.hslit
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
  br $end_integer_value
)
  br $end_literal
)
  local.get $v
  drop
  i32.const 1
  i32.const 5
  local.get $s
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
  call $hydra.haskell.utils.hslit
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
  br $end_literal
)
)
  (func $hydra.haskell.coder.encode_projection (param $namespaces i32) (param $proj i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $dn i32)
  (local $fname i32)
  local.get $proj
  i32.load
  local.set $dn
  local.get $proj
  i32.load offset=4
  local.set $fname
  i32.const 1
  i32.const 17
  local.get $namespaces
  local.get $dn
  local.get $fname
  call $hydra.haskell.utils.record_field_reference
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
  (func $hydra.haskell.coder.encode_standalone_cases (param $depth i32) (param $namespaces i32) (param $stmt i32) (param $cx i32) (param $g i32) (result i32)
  i32.const 1860
  call $hydra.haskell.utils.raw_name
  i32.const 0
  call $hydra.haskell.utils.hslambda
  local.get $depth
  local.get $namespaces
  local.get $stmt
  i32.const 1860
  call $hydra.haskell.utils.hsvar
  local.get $cx
  local.get $g
  call $hydra.haskell.coder.encode_case_expression
  call $hydra.lib.eithers.map
)
  (func $hydra.haskell.coder.encode_term (param $depth i32) (param $namespaces i32) (param $term i32) (param $cx i32) (param $g i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $abs i32)
  (local $all_bindings i32)
  (local $app i32)
  (local $arg i32)
  (local $binding i32)
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
  (local $injection i32)
  (local $inner_lt i32)
  (local $inner_result i32)
  (local $k i32)
  (local $l i32)
  (local $lam i32)
  (local $let_term i32)
  (local $lhs i32)
  (local $lt i32)
  (local $m i32)
  (local $name i32)
  (local $nonempty_map i32)
  (local $nonempty_set i32)
  (local $p i32)
  (local $pair i32)
  (local $proj i32)
  (local $r i32)
  (local $record i32)
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
  (local $typed i32)
  (local $updates i32)
  (local $v i32)
  (local $wrapped i32)
  (local $x i32)
  local.get $depth
  local.get $namespaces
  local.get $t
  local.get $cx
  local.get $g
  call $hydra.haskell.coder.encode_term
  local.set $encode
  i32.const 1373
  call $hydra.haskell.utils.hsvar
  local.set $lhs
  local.get $pair
  call $hydra.lib.pairs.first
  local.set $k
  local.get $pair
  call $hydra.lib.pairs.second
  local.set $v
  local.get $k
  drop
  local.get $encode
  drop
  i32.const 0
  local.get $v
  drop
  local.get $encode
  drop
  i32.const 0
  i32.const 1
  i32.const 14
  local.get $hk
  local.get $hv
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.set $encode_pair
  i32.const 10
  local.get $x
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
  local.get $encode_pair
  local.get $m
  call $hydra.lib.maps.to_list
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
  i32.const 1
  local.get $lhs
  local.get $rhs
  call $hydra.haskell.utils.hsapp
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
  local.set $nonempty_map
  i32.const 1479
  call $hydra.haskell.utils.hsvar
  local.set $lhs
  local.get $depth
  local.get $namespaces
  i32.const 7
  local.get $s
  call $hydra.lib.sets.to_list
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
  local.get $cx
  local.get $g
  call $hydra.haskell.coder.encode_term
  i32.const 1
  local.get $lhs
  local.get $rhs
  call $hydra.haskell.utils.hsapp
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
  local.set $nonempty_set
  (block $end_term (result i32)
  (block $wrap
  (block $variable
  (block $unit
  (block $inject
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
  (block $unwrap
  (block $project
  (block $lambda
  (block $either
  (block $cases
  (block $application
  local.get $term
  call $hydra.strip.deannotate_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $application $cases $either $lambda $project $unwrap $let $list $literal $map $maybe $pair $record $set $type_lambda $type_application $inject $unit $variable $wrap $wrap
)
  local.get $v
  drop
  local.get $app
  i32.load
  local.set $fun
  local.get $app
  i32.load offset=4
  local.set $arg
  local.get $fun
  call $hydra.strip.deannotate_term
  local.set $deannotated_fun
  (block $end_term (result i32)
  (block $cases
  local.get $deannotated_fun
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $cases $cases
)
  local.get $v
  drop
  local.get $arg
  drop
  local.get $encode
  drop
  i32.const 0
  local.get $depth
  local.get $namespaces
  local.get $stmt
  local.get $harg
  local.get $cx
  local.get $g
  call $hydra.haskell.coder.encode_case_expression
  call $hydra.lib.eithers.bind
  br $end_term
)
  br $end_term
)
  local.get $v
  drop
  local.get $depth
  local.get $namespaces
  local.get $stmt
  local.get $cx
  local.get $g
  call $hydra.haskell.coder.encode_standalone_cases
  br $end_term
)
  local.get $v
  drop
  local.get $l
  drop
  local.get $encode
  drop
  i32.const 0
  i32.const 1
  i32.const 1273
  call $hydra.haskell.utils.hsvar
  local.get $hl
  call $hydra.haskell.utils.hsapp
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
  local.get $r
  drop
  local.get $encode
  drop
  i32.const 0
  i32.const 1
  i32.const 1445
  call $hydra.haskell.utils.hsvar
  local.get $hr
  call $hydra.haskell.utils.hsapp
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
  local.get $e
  call $hydra.lib.eithers.either
  br $end_term
)
  local.get $v
  drop
  local.get $depth
  local.get $namespaces
  local.get $lam
  local.get $cx
  local.get $g
  call $hydra.haskell.coder.encode_lambda_term
  br $end_term
)
  local.get $v
  drop
  local.get $namespaces
  local.get $proj
  call $hydra.haskell.coder.encode_projection
  br $end_term
)
  local.get $v
  drop
  local.get $namespaces
  local.get $name
  call $hydra.haskell.coder.encode_unwrap
  br $end_term
)
  local.get $v
  drop
  local.get $lt
  i32.load
  local.set $bs
  local.get $lt
  i32.load offset=4
  local.set $body
  (block $end_term (result i32)
  (block $let
  local.get $body
  call $hydra.strip.deannotate_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $let $let
)
  local.get $v
  drop
  local.get $inner_lt
  drop
  local.get $collect_bindings
  drop
  i32.const 0
  local.set $inner_result
  local.get $bs
  local.get $inner_result
  call $hydra.lib.pairs.first
  call $hydra.lib.lists.concat2
  local.get $inner_result
  call $hydra.lib.pairs.second
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
  br $end_term
)
  local.set $collect_bindings
  local.get $let_term
  drop
  local.get $collect_bindings
  drop
  i32.const 0
  local.set $collected
  local.get $collected
  call $hydra.lib.pairs.first
  local.set $all_bindings
  local.get $collected
  call $hydra.lib.pairs.second
  local.set $final_body
  local.get $binding
  i32.load
  local.set $name
  local.get $binding
  i32.load offset=4
  local.set $term'
  local.get $name
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.haskell.utils.simple_name
  local.set $hname
  local.get $term'
  drop
  local.get $encode
  drop
  i32.const 0
  i32.const 1
  i32.const 1
  local.get $hname
  local.get $hexpr
  i32.const 0
  call $hydra.haskell.utils.simple_value_binding
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
  call $hydra.lib.eithers.bind
  local.set $encode_binding
  local.get $encode_binding
  local.get $all_bindings
  call $hydra.lib.eithers.map_list
  local.get $final_body
  drop
  local.get $encode
  drop
  i32.const 0
  i32.const 1
  i32.const 9
  local.get $hbindings
  local.get $hinner
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $v
  drop
  local.get $encode
  local.get $els
  call $hydra.lib.eithers.map_list
  i32.const 1
  i32.const 10
  local.get $helems
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
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $v
  drop
  local.get $v
  local.get $cx
  call $hydra.haskell.coder.encode_literal
  br $end_term
)
  local.get $v
  drop
  local.get $m
  call $hydra.lib.maps.null
  i32.const 1
  i32.const 1362
  call $hydra.haskell.utils.hsvar
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
  local.get $m
  drop
  local.get $nonempty_map
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
  br $end_term
)
  local.get $v
  drop
  local.get $m
  i32.const 1
  i32.const 1396
  call $hydra.haskell.utils.hsvar
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
  local.get $t
  drop
  local.get $encode
  drop
  i32.const 0
  i32.const 1
  i32.const 1265
  call $hydra.haskell.utils.hsvar
  local.get $ht
  call $hydra.haskell.utils.hsapp
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
  call $hydra.lib.maybes.cases
  br $end_term
)
  local.get $v
  drop
  local.get $p
  call $hydra.lib.pairs.first
  drop
  local.get $encode
  drop
  i32.const 0
  local.get $p
  call $hydra.lib.pairs.second
  drop
  local.get $encode
  drop
  i32.const 0
  i32.const 1
  i32.const 14
  local.get $f
  local.get $s
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $v
  drop
  local.get $record
  i32.load
  local.set $sname
  local.get $record
  i32.load offset=4
  local.set $fields
  local.get $field
  i32.load
  local.set $fn
  local.get $field
  i32.load offset=4
  local.set $ft
  local.get $namespaces
  local.get $sname
  local.get $fn
  call $hydra.haskell.utils.record_field_reference
  local.set $field_ref
  local.get $ft
  drop
  local.get $encode
  drop
  i32.const 0
  i32.const 1
  local.get $field_ref
  local.get $hft
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
  call $hydra.lib.eithers.bind
  local.set $to_field_update
  local.get $namespaces
  local.get $sname
  call $hydra.haskell.utils.element_reference
  local.set $type_name
  local.get $to_field_update
  local.get $fields
  call $hydra.lib.eithers.map_list
  i32.const 1
  i32.const 2
  local.get $type_name
  local.get $updates
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
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $v
  drop
  local.get $s
  call $hydra.lib.sets.null
  i32.const 1
  i32.const 1468
  call $hydra.haskell.utils.hsvar
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
  local.get $s
  drop
  local.get $nonempty_set
  drop
  i32.const 0
  call $hydra.lib.logic.if_else
  br $end_term
)
  local.get $v
  drop
  local.get $abs
  i32.load offset=4
  local.set $term1
  local.get $term1
  drop
  local.get $encode
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $typed
  i32.load
  local.set $term1
  local.get $term1
  drop
  local.get $encode
  drop
  i32.const 0
  br $end_term
)
  local.get $v
  drop
  local.get $injection
  i32.load
  local.set $sname
  local.get $injection
  i32.load offset=4
  local.set $field
  local.get $field
  i32.load
  local.set $fn
  local.get $field
  i32.load offset=4
  local.set $ft
  i32.const 17
  local.get $g
  i32.load
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  local.get $g
  i32.load offset=24
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  call $hydra.lib.sets.union
  local.get $namespaces
  local.get $sname
  local.get $fn
  call $hydra.haskell.utils.union_field_reference
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
  local.set $lhs
  local.get $lhs
  i32.const 0
  call $hydra.haskell.utils.hsapp
  local.get $ft
  drop
  local.get $encode
  drop
  i32.const 0
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
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $unit $unit
)
  local.get $v
  drop
  i32.const 1
  local.get $lhs
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
  br $end_type
)
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $v
  drop
  i32.const 1
  i32.const 14
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
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
  br $end_term
)
  local.get $v
  drop
  i32.const 1
  i32.const 17
  local.get $namespaces
  local.get $name
  call $hydra.haskell.utils.element_reference
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
  br $end_term
)
  local.get $v
  drop
  local.get $wrapped
  i32.load
  local.set $tname
  local.get $wrapped
  i32.load offset=4
  local.set $term'
  i32.const 17
  local.get $namespaces
  local.get $tname
  call $hydra.haskell.utils.element_reference
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
  local.set $lhs
  local.get $term'
  drop
  local.get $encode
  drop
  i32.const 0
  i32.const 1
  local.get $lhs
  local.get $rhs
  call $hydra.haskell.utils.hsapp
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
  br $end_term
)
)
  (func $hydra.haskell.coder.encode_type (param $namespaces i32) (param $typ i32) (param $cx i32) (param $g i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $app i32)
  (local $body i32)
  (local $cod i32)
  (local $dom i32)
  (local $either_type i32)
  (local $encode i32)
  (local $f i32)
  (local $forall_type i32)
  (local $ft i32)
  (local $fun_type i32)
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
  (local $map_type i32)
  (local $name i32)
  (local $ot i32)
  (local $pt i32)
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
  call $hydra.haskell.coder.encode_type
  local.set $encode
  i32.const 1
  i32.const 7
  local.get $namespaces
  local.get $name
  call $hydra.haskell.utils.element_reference
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
  local.set $ref
  i32.const 6
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
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
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $application $either $function $forall $list $literal $map $maybe $pair $record $set $union $unit $variable $void $wrap $wrap
)
  local.get $v
  drop
  local.get $app
  i32.load
  local.set $lhs
  local.get $app
  i32.load offset=4
  local.set $rhs
  local.get $lhs
  drop
  local.get $encode
  drop
  i32.const 0
  local.get $rhs
  drop
  local.get $encode
  drop
  i32.const 0
  i32.const 1
  local.get $hlhs
  local.get $hrhs
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
  call $hydra.haskell.utils.to_type_application
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
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $v
  drop
  local.get $either_type
  i32.load
  local.set $left'
  local.get $either_type
  i32.load offset=4
  local.set $right'
  local.get $left'
  drop
  local.get $encode
  drop
  i32.const 0
  local.get $right'
  drop
  local.get $encode
  drop
  i32.const 0
  i32.const 1
  i32.const 7
  i32.const 1146
  call $hydra.haskell.utils.raw_name
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
  local.get $hleft
  local.get $hright
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
  call $hydra.haskell.utils.to_type_application
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
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $v
  drop
  local.get $fun_type
  i32.load
  local.set $dom
  local.get $fun_type
  i32.load offset=4
  local.set $cod
  local.get $dom
  drop
  local.get $encode
  drop
  i32.const 0
  local.get $cod
  drop
  local.get $encode
  drop
  i32.const 0
  i32.const 1
  i32.const 2
  local.get $hdom
  local.get $hcod
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $v
  drop
  local.get $forall_type
  i32.load
  local.set $v
  local.get $forall_type
  i32.load offset=4
  local.set $body
  local.get $body
  drop
  local.get $encode
  drop
  i32.const 0
  br $end_type
)
  local.get $v
  drop
  local.get $lt
  drop
  local.get $encode
  drop
  i32.const 0
  i32.const 1
  i32.const 4
  local.get $hlt
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
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $v
  drop
  (block $end_literal_type (result i32)
  (block $string
  (block $integer
  (block $float
  (block $decimal
  (block $boolean
  (block $binary
  local.get $lt
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $binary $boolean $decimal $float $integer $string $string
)
  local.get $v
  drop
  i32.const 1
  i32.const 7
  i32.const 1038
  call $hydra.haskell.utils.raw_name
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
  br $end_literal_type
)
  local.get $v
  drop
  i32.const 1
  i32.const 7
  i32.const 1054
  call $hydra.haskell.utils.raw_name
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
  br $end_literal_type
)
  local.get $v
  drop
  i32.const 1
  i32.const 7
  i32.const 1500
  call $hydra.haskell.utils.raw_name
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
  br $end_literal_type
)
  local.get $v
  drop
  (block $end_float_type (result i32)
  (block $bigfloat
  (block $float64
  (block $float32
  local.get $ft
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $float32 $float64 $bigfloat $bigfloat
)
  local.get $v
  drop
  i32.const 1
  i32.const 7
  i32.const 1179
  call $hydra.haskell.utils.raw_name
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
  br $end_float_type
)
  local.get $v
  drop
  i32.const 1
  i32.const 7
  i32.const 1136
  call $hydra.haskell.utils.raw_name
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
  br $end_float_type
)
  local.get $v
  drop
  i32.const 1
  i32.const 7
  i32.const 1136
  call $hydra.haskell.utils.raw_name
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
  br $end_float_type
)
  br $end_literal_type
)
  local.get $v
  drop
  (block $end_integer_type (result i32)
  (block $int64
  (block $int32
  (block $int16
  (block $int8
  (block $bigint
  local.get $it
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $bigint $int8 $int16 $int32 $int64 $int64
)
  local.get $v
  drop
  i32.const 1
  i32.const 7
  i32.const 1254
  call $hydra.haskell.utils.raw_name
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
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 1
  i32.const 7
  i32.const 1237
  call $hydra.haskell.utils.raw_name
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
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 1
  i32.const 7
  i32.const 1215
  call $hydra.haskell.utils.raw_name
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
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 1
  i32.const 7
  i32.const 1247
  call $hydra.haskell.utils.raw_name
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
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 1
  i32.const 7
  i32.const 1226
  call $hydra.haskell.utils.raw_name
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
  br $end_integer_type
)
  br $end_literal_type
)
  local.get $v
  drop
  i32.const 1
  i32.const 7
  i32.const 1526
  call $hydra.haskell.utils.raw_name
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
  br $end_literal_type
)
  br $end_type
)
  local.get $v
  drop
  local.get $map_type
  i32.load
  local.set $kt
  local.get $map_type
  i32.load offset=4
  local.set $vt
  local.get $kt
  drop
  local.get $encode
  drop
  i32.const 0
  local.get $vt
  drop
  local.get $encode
  drop
  i32.const 0
  i32.const 1
  i32.const 7
  i32.const 1353
  call $hydra.haskell.utils.raw_name
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
  local.get $hkt
  local.get $hvt
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
  call $hydra.haskell.utils.to_type_application
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
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $v
  drop
  local.get $ot
  drop
  local.get $encode
  drop
  i32.const 0
  i32.const 1
  i32.const 7
  i32.const 1387
  call $hydra.haskell.utils.raw_name
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
  local.get $hot
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
  call $hydra.haskell.utils.to_type_application
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
  br $end_type
)
  local.get $v
  drop
  local.get $pt
  i32.load
  drop
  local.get $encode
  drop
  i32.const 0
  local.get $pt
  i32.load offset=4
  drop
  local.get $encode
  drop
  i32.const 0
  i32.const 1
  i32.const 6
  local.get $f
  local.get $s
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $v
  drop
  i32.const 1694
  drop
  local.get $ref
  drop
  i32.const 0
  br $end_type
)
  local.get $v
  drop
  local.get $st
  drop
  local.get $encode
  drop
  i32.const 0
  i32.const 1
  i32.const 7
  i32.const 1459
  call $hydra.haskell.utils.raw_name
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
  local.get $hst
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
  call $hydra.haskell.utils.to_type_application
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
  br $end_type
)
  local.get $v
  drop
  i32.const 1694
  drop
  local.get $ref
  drop
  i32.const 0
  br $end_type
)
  local.get $v
  drop
  i32.const 1
  local.get $unit_tuple
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
  br $end_type
)
  local.get $v
  drop
  local.get $v1
  drop
  local.get $ref
  drop
  i32.const 0
  br $end_type
)
  local.get $v
  drop
  i32.const 1
  i32.const 7
  i32.const 1544
  call $hydra.haskell.utils.raw_name
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
  br $end_type
)
  local.get $v
  drop
  i32.const 1694
  drop
  local.get $ref
  drop
  i32.const 0
  br $end_type
)
)
  (func $hydra.haskell.coder.encode_type_with_class_assertions (param $namespaces i32) (param $explicit_classes i32) (param $typ i32) (param $cx i32) (param $g i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
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
  (local $v i32)
  local.get $explicit_classes
  local.get $typ
  call $hydra.haskell.coder.get_implicit_type_classes
  call $hydra.lib.maps.union
  local.set $classes
  local.get $typ
  call $hydra.haskell.coder.get_implicit_type_classes
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
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $equality $ordering $ordering
)
  local.get $v
  drop
  i32.const 1164
  br $end_type_class
)
  local.get $v
  drop
  i32.const 1407
  br $end_type_class
)
  call $hydra.haskell.utils.raw_name
  local.set $hname
  i32.const 7
  local.get $name
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.haskell.utils.raw_name
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
  local.set $htype
  i32.const 0
  local.get $hname
  local.get $htype
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
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
  call $hydra.haskell.coder.adapt_type_to_haskell_and_encode
  local.get $assert_pairs
  call $hydra.lib.lists.null
  i32.const 1
  local.get $htyp
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
  local.get $encode_assertion
  local.get $assert_pairs
  call $hydra.lib.lists.map
  local.set $encoded
  local.get $encoded
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.equality.equal
  i32.const 1
  local.get $encoded
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
  local.get $encoded
  call $hydra.lib.lists.maybe_head
  call $hydra.lib.maybes.from_maybe
  i32.const 1
  local.get $encoded
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
  call $hydra.lib.logic.if_else
  local.set $hassert
  i32.const 1
  i32.const 1
  local.get $hassert
  local.get $htyp
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
  call $hydra.lib.logic.if_else
  call $hydra.lib.eithers.bind
)
  (func $hydra.haskell.coder.encode_unwrap (param $namespaces i32) (param $name i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1
  i32.const 17
  local.get $namespaces
  i32.const 1024
  local.get $name
  call $hydra.names.namespace_of
  call $hydra.lib.maybes.from_maybe
  local.get $name
  call $hydra.haskell.utils.newtype_accessor_name
  call $hydra.names.qname
  call $hydra.haskell.utils.element_reference
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
  (func $hydra.haskell.coder.extend_meta_for_term (param $meta i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $v i32)
  (block $end_term (result i32)
  (block $set
  (block $map
  local.get $term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $map $set $set
)
  local.get $v
  drop
  i32.const 1
  local.get $meta
  call $hydra.haskell.coder.set_meta_uses_map
  br $end_term
)
  local.get $v
  drop
  i32.const 1
  local.get $meta
  call $hydra.haskell.coder.set_meta_uses_set
  br $end_term
)
)
  (func $hydra.haskell.coder.extend_meta_for_type (param $meta i32) (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $it i32)
  (local $lt i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $set
  (block $map
  (block $literal
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $literal $map $set $set
)
  local.get $v
  drop
  (block $end_literal_type (result i32)
  (block $integer
  (block $binary
  local.get $lt
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $binary $integer $integer
)
  local.get $v
  drop
  i32.const 1
  local.get $meta
  call $hydra.haskell.coder.set_meta_uses_byte_string
  br $end_literal_type
)
  local.get $v
  drop
  (block $end_integer_type (result i32)
  (block $int64
  (block $int16
  (block $int8
  local.get $it
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $int8 $int16 $int64 $int64
)
  local.get $v
  drop
  i32.const 1
  local.get $meta
  call $hydra.haskell.coder.set_meta_uses_int
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 1
  local.get $meta
  call $hydra.haskell.coder.set_meta_uses_int
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 1
  local.get $meta
  call $hydra.haskell.coder.set_meta_uses_int
  br $end_integer_type
)
  br $end_literal_type
)
  br $end_type
)
  local.get $v
  drop
  i32.const 1
  local.get $meta
  call $hydra.haskell.coder.set_meta_uses_map
  br $end_type
)
  local.get $v
  drop
  i32.const 1
  local.get $meta
  call $hydra.haskell.coder.set_meta_uses_set
  br $end_type
)
)
  (func $hydra.haskell.coder.find_ord_variables (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $et i32)
  (local $fold i32)
  (local $is_type_variable i32)
  (local $kt i32)
  (local $map_type i32)
  (local $names i32)
  (local $t i32)
  (local $try_type i32)
  (local $typ' i32)
  (local $v i32)
  (block $end_type (result i32)
  (block $set
  (block $map
  local.get $typ'
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $map $set $set
)
  local.get $v
  drop
  local.get $map_type
  i32.load
  local.set $kt
  local.get $names
  drop
  local.get $kt
  drop
  local.get $try_type
  drop
  i32.const 0
  br $end_type
)
  local.get $v
  drop
  local.get $names
  drop
  local.get $et
  drop
  local.get $try_type
  drop
  i32.const 0
  br $end_type
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
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $variable $variable
)
  local.get $v
  drop
  local.get $v
  drop
  local.get $is_type_variable
  drop
  i32.const 0
  local.get $v
  local.get $names
  call $hydra.lib.sets.insert
  local.get $names
  call $hydra.lib.logic.if_else
  br $end_type
)
  local.set $try_type
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
  local.get $fold
  i32.const 0
  local.get $typ
  call $hydra.rewriting.fold_over_type
)
  (func $hydra.haskell.coder.gather_metadata (param $defs i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $add_def i32)
  (local $def i32)
  (local $m i32)
  (local $meta i32)
  (local $meta_with_term i32)
  (local $t i32)
  (local $term i32)
  (local $term_def i32)
  (local $ts i32)
  (local $typ i32)
  (local $type_def i32)
  (local $v i32)
  (block $end_definition (result i32)
  (block $type
  (block $term
  local.get $def
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $term $type $type
)
  local.get $v
  drop
  local.get $term_def
  i32.load offset=4
  local.set $term
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
  local.get $m
  local.get $t
  call $hydra.haskell.coder.extend_meta_for_term
  local.get $meta
  local.get $term
  call $hydra.rewriting.fold_over_term
  local.set $meta_with_term
  local.get $meta_with_term
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
  local.get $m
  local.get $t
  call $hydra.haskell.coder.extend_meta_for_type
  local.get $meta_with_term
  local.get $ts
  i32.load offset=4
  call $hydra.rewriting.fold_over_type
  local.get $term_def
  i32.load offset=8
  call $hydra.lib.maybes.maybe
  br $end_definition
)
  local.get $v
  drop
  local.get $type_def
  i32.load offset=4
  i32.load offset=4
  local.set $typ
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
  local.get $m
  local.get $t
  call $hydra.haskell.coder.extend_meta_for_type
  local.get $meta
  local.get $typ
  call $hydra.rewriting.fold_over_type
  br $end_definition
)
  local.set $add_def
  local.get $add_def
  i32.const 0
  local.get $defs
  call $hydra.lib.lists.foldl
)
  (func $hydra.haskell.coder.get_implicit_type_classes (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $name i32)
  (local $to_pair i32)
  local.get $name
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.sets.from_list
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
  local.set $to_pair
  local.get $to_pair
  local.get $typ
  call $hydra.haskell.coder.find_ord_variables
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
)
  (func $hydra.haskell.coder.include_type_definitions (result i32)
  i32.const 0
)
  (func $hydra.haskell.coder.key_haskell_var (result i32)
  i32.const 1617
)
  (func $hydra.haskell.coder.module_to_haskell (param $mod i32) (param $defs i32) (param $cx i32) (param $g i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $filepath i32)
  (local $hsmod i32)
  (local $s i32)
  local.get $mod
  local.get $defs
  local.get $cx
  local.get $g
  call $hydra.haskell.coder.module_to_haskell_module
  local.get $hsmod
  call $hydra.haskell.serde.module_to_expr
  call $hydra.serialization.parenthesize
  call $hydra.serialization.print_expr
  local.set $s
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
  i32.const 1631
  local.get $mod
  i32.load
  call $hydra.names.namespace_to_file_path
  local.set $filepath
  i32.const 1
  local.get $filepath
  local.get $s
  call $hydra.lib.maps.singleton
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
  (func $hydra.haskell.coder.module_to_haskell_module (param $mod i32) (param $defs i32) (param $cx i32) (param $g i32) (result i32)
  (local $namespaces i32)
  local.get $mod
  local.get $cx
  local.get $g
  call $hydra.haskell.utils.namespaces_for_module
  local.get $namespaces
  local.get $mod
  local.get $defs
  local.get $cx
  local.get $g
  call $hydra.haskell.coder.construct_module
  call $hydra.lib.eithers.bind
)
  (func $hydra.haskell.coder.name_decls (param $namespaces i32) (param $name i32) (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $decl i32)
  (local $field_decls i32)
  (local $field_type i32)
  (local $fname i32)
  (local $k i32)
  (local $n i32)
  (local $name_decl i32)
  (local $nm i32)
  (local $pair i32)
  (local $to_constant i32)
  (local $to_decl i32)
  (local $v i32)
  local.get $name
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $nm
  local.get $pair
  call $hydra.lib.pairs.first
  local.set $k
  local.get $pair
  call $hydra.lib.pairs.second
  local.set $v
  i32.const 2
  i32.const 0
  local.get $k
  call $hydra.haskell.utils.simple_name
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  call $hydra.haskell.utils.application_pattern
  i32.const 0
  i32.const 17
  local.get $namespaces
  local.get $n
  call $hydra.haskell.utils.element_reference
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
  i32.const 6
  i32.const 5
  local.get $v
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
  local.set $decl
  local.get $decl
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
  local.set $to_decl
  local.get $name
  call $hydra.haskell.coder.constant_for_type_name
  local.get $nm
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
  local.set $name_decl
  local.get $to_constant
  local.get $typ
  call $hydra.lexical.fields_of
  call $hydra.lib.lists.map
  local.set $field_decls
  local.get $field_type
  i32.load
  local.set $fname
  local.get $name
  local.get $fname
  call $hydra.haskell.coder.constant_for_field_name
  local.get $fname
  drop
  i32.const 0
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
  local.set $to_constant
  i32.const 0
  i32.const 1637
  drop
  local.get $name_decl
  drop
  local.get $to_decl
  drop
  i32.const 0
  i32.const 1637
  drop
  local.get $to_decl
  drop
  i32.const 0
  local.get $field_decls
  call $hydra.lib.lists.map
  call $hydra.lib.lists.cons
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  call $hydra.lib.logic.if_else
)
  (func $hydra.haskell.coder.set_meta_uses_byte_string (param $b i32) (param $m i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  local.get $b
  local.get $m
  i32.load offset=4
  local.get $m
  i32.load offset=8
  local.get $m
  i32.load offset=12
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
)
  (func $hydra.haskell.coder.set_meta_uses_int (param $b i32) (param $m i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  local.get $m
  i32.load
  local.get $b
  local.get $m
  i32.load offset=8
  local.get $m
  i32.load offset=12
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
)
  (func $hydra.haskell.coder.set_meta_uses_map (param $b i32) (param $m i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  local.get $m
  i32.load
  local.get $m
  i32.load offset=4
  local.get $b
  local.get $m
  i32.load offset=12
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
)
  (func $hydra.haskell.coder.set_meta_uses_set (param $b i32) (param $m i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  local.get $m
  i32.load
  local.get $m
  i32.load offset=4
  local.get $m
  i32.load offset=8
  local.get $b
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
)
  (func $hydra.haskell.coder.to_data_declaration (param $namespaces i32) (param $def i32) (param $cx i32) (param $g i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $all_bindings i32)
  (local $app_pat i32)
  (local $args i32)
  (local $binding i32)
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
  (local $lambda' i32)
  (local $lb i32)
  (local $lbindings i32)
  (local $let_term i32)
  (local $name i32)
  (local $name' i32)
  (local $new_pattern i32)
  (local $new_rhs i32)
  (local $pattern' i32)
  (local $prev_bindings i32)
  (local $rewrite_value_binding i32)
  (local $rhs i32)
  (local $rhs_expr i32)
  (local $simple i32)
  (local $t i32)
  (local $term i32)
  (local $term' i32)
  (local $terms i32)
  (local $to_decl i32)
  (local $to_term_definition i32)
  (local $typ i32)
  (local $v i32)
  (local $vars i32)
  (local $vb i32)
  local.get $def
  i32.load
  local.set $name
  local.get $def
  i32.load offset=4
  local.set $term
  local.get $def
  i32.load offset=8
  local.set $typ
  local.get $name
  call $hydra.names.local_name_of
  call $hydra.haskell.utils.simple_name
  local.set $hname
  (block $end_value_binding (result i32)
  (block $simple
  local.get $vb
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $simple $simple
)
  local.get $v
  drop
  local.get $simple
  i32.load
  local.set $pattern'
  local.get $simple
  i32.load offset=4
  local.set $rhs
  local.get $simple
  i32.load offset=8
  local.set $bindings
  (block $end_pattern (result i32)
  (block $application
  local.get $pattern'
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $application $application
)
  local.get $v
  drop
  local.get $app_pat
  i32.load
  local.set $name'
  local.get $app_pat
  i32.load offset=4
  local.set $args
  local.get $rhs
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $rhs_expr
  (block $end_expression (result i32)
  (block $lambda
  local.get $rhs_expr
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $lambda $lambda
)
  local.get $v
  drop
  local.get $lambda'
  i32.load
  local.set $vars
  local.get $lambda'
  i32.load offset=4
  local.set $body
  local.get $name'
  local.get $args
  local.get $vars
  call $hydra.lib.lists.concat2
  call $hydra.haskell.utils.application_pattern
  local.set $new_pattern
  local.get $body
  local.set $new_rhs
  i32.const 0
  local.get $new_pattern
  local.get $new_rhs
  local.get $bindings
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
  drop
  local.get $rewrite_value_binding
  drop
  i32.const 0
  br $end_expression
)
  br $end_pattern
)
  br $end_value_binding
)
  local.set $rewrite_value_binding
  (block $end_term (result i32)
  (block $let
  local.get $term'
  call $hydra.strip.deannotate_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $let $let
)
  local.get $v
  drop
  local.get $let_term
  i32.load
  local.set $lbindings
  local.get $let_term
  i32.load offset=4
  local.set $env
  i32.const 1
  local.get $hname''
  local.get $hterm'
  i32.const 0
  call $hydra.haskell.utils.simple_value_binding
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
  local.set $to_term_definition
  local.get $binding
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.haskell.utils.simple_name
  local.get $lbindings
  call $hydra.lib.lists.map
  local.set $hnames
  i32.const 0
  local.get $lbindings
  call $hydra.lib.lists.map
  local.set $terms
  i32.const 0
  local.get $namespaces
  local.get $t
  local.get $cx
  local.get $g
  call $hydra.haskell.coder.encode_term
  local.get $terms
  call $hydra.lib.eithers.map_list
  local.get $to_term_definition
  local.get $hnames
  local.get $hterms
  call $hydra.lib.lists.zip_with
  local.set $hbindings
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $lb
  drop
  i32.const 0
  drop
  i32.const 0
  local.get $bindings
  call $hydra.lib.maybes.maybe
  local.set $prev_bindings
  local.get $prev_bindings
  local.get $hbindings
  call $hydra.lib.lists.concat2
  local.set $all_bindings
  local.get $comments
  drop
  local.get $hname'
  drop
  local.get $env
  drop
  local.get $all_bindings
  drop
  local.get $to_decl
  drop
  i32.const 0
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.set $to_decl
  local.get $cx
  local.get $g
  local.get $term
  call $hydra.annotations.get_term_description
  local.get $comments
  drop
  local.get $hname
  drop
  local.get $term
  drop
  i32.const 0
  drop
  local.get $to_decl
  drop
  i32.const 0
  call $hydra.lib.eithers.bind
)
  (func $hydra.haskell.coder.to_type_declarations_from (param $namespaces i32) (param $element_name i32) (param $typ i32) (param $cx i32) (param $g i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $bound_names' i32)
  (local $comments i32)
  (local $cons i32)
  (local $constructor_name i32)
  (local $decl i32)
  (local $decl' i32)
  (local $decl_head i32)
  (local $deconflict i32)
  (local $deriv i32)
  (local $field_type i32)
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
  (local $p i32)
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
  (local $v i32)
  (local $vars i32)
  (local $vars' i32)
  (local $wrapped i32)
  local.get $element_name
  call $hydra.names.local_name_of
  local.set $lname
  local.get $lname
  call $hydra.haskell.utils.simple_name
  local.set $hname
  i32.const 2
  local.get $name
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
  local.get $p
  call $hydra.lib.pairs.first
  local.set $h
  local.get $p
  call $hydra.lib.pairs.second
  local.set $rest
  local.get $h
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.haskell.utils.simple_name
  local.set $hvar
  i32.const 0
  local.get $name
  drop
  local.get $rest
  drop
  local.get $decl_head
  drop
  i32.const 0
  local.get $hvar
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
  local.get $vars'
  call $hydra.lib.lists.uncons
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.from_maybe
  local.set $decl_head
  local.get $tname
  call $hydra.haskell.utils.newtype_accessor_name
  call $hydra.haskell.utils.simple_name
  local.set $hname0
  local.get $namespaces
  local.get $typ'
  local.get $cx
  local.get $g
  call $hydra.haskell.coder.adapt_type_to_haskell_and_encode
  local.get $hname0
  local.get $htype
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
  local.set $hfield
  local.get $tname
  call $hydra.names.local_name_of
  call $hydra.haskell.utils.simple_name
  local.set $constructor_name
  i32.const 1
  i32.const 1
  local.get $constructor_name
  local.get $hfield
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
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
  call $hydra.lib.eithers.bind
  local.set $newtype_cons
  local.get $field_type
  i32.load
  local.set $fname
  local.get $field_type
  i32.load offset=4
  local.set $ftype
  local.get $lname'
  call $hydra.formatting.decapitalize
  local.get $fname
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.formatting.capitalize
  call $hydra.lib.strings.cat2
  call $hydra.haskell.utils.simple_name
  local.set $hname'
  local.get $namespaces
  local.get $ftype
  local.get $cx
  local.get $g
  call $hydra.haskell.coder.adapt_type_to_haskell_and_encode
  local.get $cx
  local.get $g
  local.get $ftype
  call $hydra.annotations.get_type_description
  i32.const 1
  local.get $hname'
  local.get $htype
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
  local.get $comments
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.set $to_field
  local.get $to_field
  local.get $fields
  call $hydra.lib.eithers.map_list
  i32.const 1
  i32.const 1
  local.get $lname'
  call $hydra.haskell.utils.simple_name
  local.get $h_fields
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
  call $hydra.lib.eithers.bind
  local.set $record_cons
  local.get $field_type
  i32.load
  local.set $fname
  local.get $field_type
  i32.load offset=4
  local.set $ftype
  local.get $namespaces
  i32.load
  call $hydra.lib.pairs.first
  local.get $name
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
  call $hydra.names.unqualify_name
  local.set $tname
  local.get $tname
  local.get $bound_names'
  call $hydra.lib.sets.member
  local.get $name
  i32.const 1552
  call $hydra.lib.strings.cat2
  drop
  local.get $deconflict
  drop
  i32.const 0
  local.get $name
  call $hydra.lib.logic.if_else
  local.set $deconflict
  local.get $cx
  local.get $g
  local.get $ftype
  call $hydra.annotations.get_type_description
  local.get $lname'
  call $hydra.formatting.capitalize
  local.get $fname
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.formatting.capitalize
  call $hydra.lib.strings.cat2
  drop
  local.get $deconflict
  drop
  i32.const 0
  local.set $nm
  local.get $ftype
  call $hydra.strip.deannotate_type
  i32.const 13
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
  call $hydra.lib.equality.equal
  i32.const 1
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
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
  local.get $namespaces
  local.get $ftype
  local.get $cx
  local.get $g
  call $hydra.haskell.coder.adapt_type_to_haskell_and_encode
  i32.const 1
  local.get $htype
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.logic.if_else
  i32.const 1
  i32.const 0
  local.get $nm
  call $hydra.haskell.utils.simple_name
  local.get $type_list
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
  local.get $comments
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.set $union_cons
  local.get $cx
  local.get $g
  local.get $element_name
  call $hydra.predicates.is_serializable_by_name
  local.get $is_ser
  i32.const 0
  i32.const 1164
  i32.const 1407
  i32.const 1437
  i32.const 1518
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
  call $hydra.lib.lists.map
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  call $hydra.lib.logic.if_else
  local.set $deriv
  local.get $typ
  call $hydra.haskell.utils.unpack_forall_type
  local.set $unpack_result
  local.get $unpack_result
  call $hydra.lib.pairs.first
  local.set $vars
  local.get $unpack_result
  call $hydra.lib.pairs.second
  local.set $t'
  local.get $hname
  drop
  local.get $vars
  call $hydra.lib.lists.reverse
  drop
  local.get $decl_head
  drop
  i32.const 0
  local.set $hd
  (block $end_type (result i32)
  (block $wrap
  (block $union
  (block $record
  local.get $t'
  call $hydra.strip.deannotate_type
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $union $wrap $wrap
)
  local.get $v
  drop
  local.get $lname
  drop
  local.get $rt
  drop
  local.get $record_cons
  drop
  i32.const 0
  i32.const 1
  i32.const 0
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $hd
  local.get $cons
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  local.get $deriv
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
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
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $v
  drop
  local.get $g
  i32.load
  call $hydra.lib.maps.keys
  call $hydra.lib.sets.from_list
  drop
  local.get $lname
  drop
  local.get $union_cons
  drop
  i32.const 0
  local.get $rt
  call $hydra.lib.eithers.map_list
  i32.const 1
  i32.const 0
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $hd
  local.get $cons
  local.get $deriv
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
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
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $v
  drop
  local.get $element_name
  drop
  local.get $wrapped
  drop
  local.get $newtype_cons
  drop
  i32.const 0
  i32.const 1
  i32.const 0
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
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  local.get $hd
  local.get $cons
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  local.get $deriv
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
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
  call $hydra.lib.eithers.bind
  br $end_type
)
  local.get $cx
  local.get $g
  local.get $typ
  call $hydra.annotations.get_type_description
  i32.const 0
  local.get $namespaces
  local.get $element_name
  local.get $typ
  local.get $cx
  local.get $g
  call $hydra.haskell.coder.type_decl
  i32.const 1
  local.get $decl'
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
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
  call $hydra.lib.eithers.bind
  i32.const 1
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
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
  call $hydra.lib.logic.if_else
  local.get $decl
  local.get $comments
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
  local.set $main_decl
  local.get $namespaces
  local.get $element_name
  local.get $typ
  call $hydra.haskell.coder.name_decls
  local.set $name_decls'
  i32.const 1
  local.get $main_decl
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  local.get $name_decls'
  local.get $tdecls
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
  call $hydra.lib.lists.concat
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.haskell.coder.type_decl (param $namespaces i32) (param $name i32) (param $typ i32) (param $cx i32) (param $g i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $decl i32)
  (local $decode_name i32)
  (local $decode_string i32)
  (local $expr i32)
  (local $field i32)
  (local $final_term i32)
  (local $fname i32)
  (local $for_type i32)
  (local $for_variable_type i32)
  (local $fterm i32)
  (local $hname i32)
  (local $inj i32)
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
  (local $v i32)
  (local $variant_result i32)
  (local $vname i32)
  (local $wt i32)
  (local $x i32)
  local.get $ns
  local.get $name'
  drop
  local.get $type_name_local
  drop
  i32.const 0
  call $hydra.names.qname
  local.set $type_name
  i32.const 1552
  local.get $name'
  call $hydra.names.local_name_of
  i32.const 1557
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
  local.set $type_name_local
  local.get $typ
  call $hydra.encode.core.type
  local.set $raw_term
  (block $end_term (result i32)
  (block $inject
  local.get $term
  call $hydra.strip.deannotate_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $inject $inject
)
  local.get $v
  drop
  local.get $inj
  i32.load
  i32.const 1656
  call $hydra.lib.equality.equal
  local.get $inj
  i32.load offset=4
  i32.const 0
  call $hydra.lib.logic.if_else
  br $end_term
)
  local.set $variant_result
  (block $end_term (result i32)
  (block $literal
  local.get $term2
  call $hydra.strip.deannotate_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $literal $literal
)
  local.get $v
  drop
  (block $end_literal (result i32)
  (block $string
  local.get $lit
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $string $string
)
  local.get $v
  drop
  local.get $s
  br $end_literal
)
  br $end_term
)
  local.set $decode_string
  (block $end_term (result i32)
  (block $wrap
  local.get $term2
  call $hydra.strip.deannotate_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $wrap $wrap
)
  local.get $v
  drop
  local.get $wt
  i32.load
  i32.const 1637
  call $hydra.lib.equality.equal
  local.get $x
  local.get $wt
  i32.load offset=4
  drop
  local.get $decode_string
  drop
  i32.const 0
  call $hydra.lib.maybes.map
  i32.const 0
  call $hydra.lib.logic.if_else
  br $end_term
)
  local.set $decode_name
  local.get $field
  i32.load
  local.set $fname
  local.get $field
  i32.load offset=4
  local.set $fterm
  local.get $fname
  i32.const 1717
  call $hydra.lib.equality.equal
  i32.const 0
  local.get $fname
  i32.const 1848
  call $hydra.lib.equality.equal
  local.get $fterm
  drop
  local.get $decode_name
  drop
  i32.const 0
  local.get $for_variable_type
  call $hydra.lib.maybes.bind
  i32.const 0
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  local.set $for_type
  local.get $vname
  call $hydra.names.qualify_name
  local.set $qname
  local.get $qname
  i32.load
  local.set $mns
  local.get $qname
  i32.load offset=4
  local.set $local
  i32.const 19
  local.get $ns
  i32.const 1552
  local.get $local
  i32.const 1557
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
  call $hydra.names.qname
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
  local.get $mns
  call $hydra.lib.maybes.map
  local.set $for_variable_type
  local.get $term
  drop
  local.get $recurse
  drop
  i32.const 0
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
  call $hydra.haskell.coder.encode_term
  local.get $expr
  local.set $rhs
  local.get $name
  drop
  local.get $type_name_local
  drop
  i32.const 0
  call $hydra.haskell.utils.simple_name
  local.set $hname
  local.get $hname
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 0
  i32.store
  local.get $__rec_ptr
  call $hydra.haskell.utils.application_pattern
  local.set $pat
  i32.const 2
  i32.const 0
  local.get $pat
  local.get $rhs
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
  local.set $decl
  i32.const 1
  local.get $decl
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
  call $hydra.lib.eithers.bind
)
  (func $hydra.haskell.coder.type_scheme_constraints_to_class_map (param $maybe_constraints i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $class_name i32)
  (local $class_name_str i32)
  (local $constraints i32)
  (local $is_eq i32)
  (local $is_ord i32)
  (local $meta i32)
  (local $name_to_type_class i32)
  local.get $class_name
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $class_name_str
  local.get $class_name_str
  i32.const 1597
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.equality.equal
  local.set $is_eq
  local.get $class_name_str
  i32.const 1682
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.equality.equal
  local.set $is_ord
  local.get $is_eq
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
  local.get $is_ord
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
  i32.const 0
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  local.set $name_to_type_class
  i32.const 0
  local.get $name_to_type_class
  local.get $meta
  i32.load
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.maybes.cat
  call $hydra.lib.sets.from_list
  local.get $constraints
  call $hydra.lib.maps.map
  local.get $maybe_constraints
  call $hydra.lib.maybes.maybe
)
  (func $hydra.haskell.coder.use_core_import (result i32)
  i32.const 1
)
)
