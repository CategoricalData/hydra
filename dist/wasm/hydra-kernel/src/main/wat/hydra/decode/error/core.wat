(module
  (import "hydra.extract.core" "hydra.extract.core.require_field" (func $hydra.extract.core.require_field (param i32) (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.strip_with_decoding_error" (func $hydra.extract.core.strip_with_decoding_error (param i32) (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.to_field_map" (func $hydra.extract.core.to_field_map (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\09\00\00\00\20\69\6e\20\75\6e\69\6f\6e\0a\00\00\00\61\63\74\75\61\6c\54\65\72\6d\0a\00\00\00\61\63\74\75\61\6c\54\79\70\65\11\00\00\00\63\6f\6e\73\74\61\6e\74\43\6f\6e\64\69\74\69\6f\6e\10\00\00\00\64\75\70\6c\69\63\61\74\65\42\69\6e\64\69\6e\67\0e\00\00\00\64\75\70\6c\69\63\61\74\65\46\69\65\6c\64\1d\00\00\00\64\75\70\6c\69\63\61\74\65\52\65\63\6f\72\64\54\79\70\65\46\69\65\6c\64\4e\61\6d\65\73\1c\00\00\00\64\75\70\6c\69\63\61\74\65\55\6e\69\6f\6e\54\79\70\65\46\69\65\6c\64\4e\61\6d\65\73\0b\00\00\00\65\6c\65\6d\65\6e\74\54\79\70\65\12\00\00\00\65\6d\70\74\79\43\61\73\65\53\74\61\74\65\6d\65\6e\74\10\00\00\00\65\6d\70\74\79\4c\65\74\42\69\6e\64\69\6e\67\73\0f\00\00\00\65\6d\70\74\79\52\65\63\6f\72\64\54\79\70\65\13\00\00\00\65\6d\70\74\79\54\65\72\6d\41\6e\6e\6f\74\61\74\69\6f\6e\13\00\00\00\65\6d\70\74\79\54\79\70\65\41\6e\6e\6f\74\61\74\69\6f\6e\13\00\00\00\65\6d\70\74\79\54\79\70\65\4e\61\6d\65\49\6e\54\65\72\6d\0e\00\00\00\65\6d\70\74\79\55\6e\69\6f\6e\54\79\70\65\18\00\00\00\65\78\70\65\63\74\65\64\20\62\6f\6f\6c\65\61\6e\20\6c\69\74\65\72\61\6c\10\00\00\00\65\78\70\65\63\74\65\64\20\6c\69\74\65\72\61\6c\0f\00\00\00\65\78\70\65\63\74\65\64\20\72\65\63\6f\72\64\0e\00\00\00\65\78\70\65\63\74\65\64\20\75\6e\69\6f\6e\0f\00\00\00\65\78\70\65\63\74\65\64\56\61\72\69\61\6e\74\09\00\00\00\66\69\65\6c\64\4e\61\6d\65\1a\00\00\00\69\6e\76\61\6c\69\64\46\6f\72\61\6c\6c\50\61\72\61\6d\65\74\65\72\4e\61\6d\65\1a\00\00\00\69\6e\76\61\6c\69\64\4c\61\6d\62\64\61\50\61\72\61\6d\65\74\65\72\4e\61\6d\65\15\00\00\00\69\6e\76\61\6c\69\64\4c\65\74\42\69\6e\64\69\6e\67\4e\61\6d\65\1e\00\00\00\69\6e\76\61\6c\69\64\54\79\70\65\4c\61\6d\62\64\61\50\61\72\61\6d\65\74\65\72\4e\61\6d\65\1d\00\00\00\69\6e\76\61\6c\69\64\54\79\70\65\53\63\68\65\6d\65\56\61\72\69\61\62\6c\65\4e\61\6d\65\07\00\00\00\6b\65\79\54\79\70\65\08\00\00\00\6c\6f\63\61\74\69\6f\6e\04\00\00\00\6e\61\6d\65\14\00\00\00\6e\65\73\74\65\64\54\65\72\6d\41\6e\6e\6f\74\61\74\69\6f\6e\14\00\00\00\6e\65\73\74\65\64\54\79\70\65\41\6e\6e\6f\74\61\74\69\6f\6e\0e\00\00\00\6e\6f\20\73\75\63\68\20\66\69\65\6c\64\20\17\00\00\00\6e\6f\6e\43\6f\6d\70\61\72\61\62\6c\65\4d\61\70\4b\65\79\54\79\70\65\1b\00\00\00\6e\6f\6e\43\6f\6d\70\61\72\61\62\6c\65\53\65\74\45\6c\65\6d\65\6e\74\54\79\70\65\13\00\00\00\72\65\64\75\6e\64\61\6e\74\57\72\61\70\55\6e\77\72\61\70\0f\00\00\00\73\65\6c\66\41\70\70\6c\69\63\61\74\69\6f\6e\12\00\00\00\73\69\6e\67\6c\65\56\61\72\69\61\6e\74\55\6e\69\6f\6e\15\00\00\00\74\65\72\6d\56\61\72\69\61\62\6c\65\53\68\61\64\6f\77\69\6e\67\08\00\00\00\74\79\70\65\4e\61\6d\65\1d\00\00\00\74\79\70\65\56\61\72\69\61\62\6c\65\53\68\61\64\6f\77\69\6e\67\49\6e\46\6f\72\61\6c\6c\21\00\00\00\74\79\70\65\56\61\72\69\61\62\6c\65\53\68\61\64\6f\77\69\6e\67\49\6e\54\79\70\65\4c\61\6d\62\64\61\15\00\00\00\75\6e\64\65\66\69\6e\65\64\54\65\72\6d\56\61\72\69\61\62\6c\65\15\00\00\00\75\6e\64\65\66\69\6e\65\64\54\79\70\65\56\61\72\69\61\62\6c\65\22\00\00\00\75\6e\64\65\66\69\6e\65\64\54\79\70\65\56\61\72\69\61\62\6c\65\49\6e\42\69\6e\64\69\6e\67\54\79\70\65\23\00\00\00\75\6e\64\65\66\69\6e\65\64\54\79\70\65\56\61\72\69\61\62\6c\65\49\6e\4c\61\6d\62\64\61\44\6f\6d\61\69\6e\26\00\00\00\75\6e\64\65\66\69\6e\65\64\54\79\70\65\56\61\72\69\61\62\6c\65\49\6e\54\79\70\65\41\70\70\6c\69\63\61\74\69\6f\6e\14\00\00\00\75\6e\6b\6e\6f\77\6e\50\72\69\6d\69\74\69\76\65\4e\61\6d\65\1e\00\00\00\75\6e\6e\65\63\65\73\73\61\72\79\49\64\65\6e\74\69\74\79\41\70\70\6c\69\63\61\74\69\6f\6e\13\00\00\00\75\6e\74\79\70\65\64\54\65\72\6d\56\61\72\69\61\62\6c\65\05\00\00\00\76\61\6c\75\65\17\00\00\00\76\6f\69\64\49\6e\4e\6f\6e\42\6f\74\74\6f\6d\50\6f\73\69\74\69\6f\6e")
  (global $__bump_ptr (mut i32) i32.const 2240 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.decode.error.core.constant_condition_error" (func $hydra.decode.error.core.constant_condition_error) )
  (export "hydra.decode.error.core.duplicate_binding_error" (func $hydra.decode.error.core.duplicate_binding_error) )
  (export "hydra.decode.error.core.duplicate_field_error" (func $hydra.decode.error.core.duplicate_field_error) )
  (export "hydra.decode.error.core.duplicate_record_type_field_names_error" (func $hydra.decode.error.core.duplicate_record_type_field_names_error) )
  (export "hydra.decode.error.core.duplicate_union_type_field_names_error" (func $hydra.decode.error.core.duplicate_union_type_field_names_error) )
  (export "hydra.decode.error.core.empty_case_statement_error" (func $hydra.decode.error.core.empty_case_statement_error) )
  (export "hydra.decode.error.core.empty_let_bindings_error" (func $hydra.decode.error.core.empty_let_bindings_error) )
  (export "hydra.decode.error.core.empty_record_type_error" (func $hydra.decode.error.core.empty_record_type_error) )
  (export "hydra.decode.error.core.empty_term_annotation_error" (func $hydra.decode.error.core.empty_term_annotation_error) )
  (export "hydra.decode.error.core.empty_type_annotation_error" (func $hydra.decode.error.core.empty_type_annotation_error) )
  (export "hydra.decode.error.core.empty_type_name_in_term_error" (func $hydra.decode.error.core.empty_type_name_in_term_error) )
  (export "hydra.decode.error.core.empty_union_type_error" (func $hydra.decode.error.core.empty_union_type_error) )
  (export "hydra.decode.error.core.invalid_forall_parameter_name_error" (func $hydra.decode.error.core.invalid_forall_parameter_name_error) )
  (export "hydra.decode.error.core.invalid_lambda_parameter_name_error" (func $hydra.decode.error.core.invalid_lambda_parameter_name_error) )
  (export "hydra.decode.error.core.invalid_let_binding_name_error" (func $hydra.decode.error.core.invalid_let_binding_name_error) )
  (export "hydra.decode.error.core.invalid_term_error" (func $hydra.decode.error.core.invalid_term_error) )
  (export "hydra.decode.error.core.invalid_type_error" (func $hydra.decode.error.core.invalid_type_error) )
  (export "hydra.decode.error.core.invalid_type_lambda_parameter_name_error" (func $hydra.decode.error.core.invalid_type_lambda_parameter_name_error) )
  (export "hydra.decode.error.core.invalid_type_scheme_variable_name_error" (func $hydra.decode.error.core.invalid_type_scheme_variable_name_error) )
  (export "hydra.decode.error.core.nested_term_annotation_error" (func $hydra.decode.error.core.nested_term_annotation_error) )
  (export "hydra.decode.error.core.nested_type_annotation_error" (func $hydra.decode.error.core.nested_type_annotation_error) )
  (export "hydra.decode.error.core.non_comparable_map_key_type_error" (func $hydra.decode.error.core.non_comparable_map_key_type_error) )
  (export "hydra.decode.error.core.non_comparable_set_element_type_error" (func $hydra.decode.error.core.non_comparable_set_element_type_error) )
  (export "hydra.decode.error.core.redundant_wrap_unwrap_error" (func $hydra.decode.error.core.redundant_wrap_unwrap_error) )
  (export "hydra.decode.error.core.self_application_error" (func $hydra.decode.error.core.self_application_error) )
  (export "hydra.decode.error.core.single_variant_union_error" (func $hydra.decode.error.core.single_variant_union_error) )
  (export "hydra.decode.error.core.term_variable_shadowing_error" (func $hydra.decode.error.core.term_variable_shadowing_error) )
  (export "hydra.decode.error.core.type_variable_shadowing_in_forall_error" (func $hydra.decode.error.core.type_variable_shadowing_in_forall_error) )
  (export "hydra.decode.error.core.type_variable_shadowing_in_type_lambda_error" (func $hydra.decode.error.core.type_variable_shadowing_in_type_lambda_error) )
  (export "hydra.decode.error.core.undefined_field_error" (func $hydra.decode.error.core.undefined_field_error) )
  (export "hydra.decode.error.core.undefined_term_variable_error" (func $hydra.decode.error.core.undefined_term_variable_error) )
  (export "hydra.decode.error.core.undefined_type_variable_error" (func $hydra.decode.error.core.undefined_type_variable_error) )
  (export "hydra.decode.error.core.undefined_type_variable_in_binding_type_error" (func $hydra.decode.error.core.undefined_type_variable_in_binding_type_error) )
  (export "hydra.decode.error.core.undefined_type_variable_in_lambda_domain_error" (func $hydra.decode.error.core.undefined_type_variable_in_lambda_domain_error) )
  (export "hydra.decode.error.core.undefined_type_variable_in_type_application_error" (func $hydra.decode.error.core.undefined_type_variable_in_type_application_error) )
  (export "hydra.decode.error.core.unexpected_term_variant_error" (func $hydra.decode.error.core.unexpected_term_variant_error) )
  (export "hydra.decode.error.core.unexpected_type_variant_error" (func $hydra.decode.error.core.unexpected_type_variant_error) )
  (export "hydra.decode.error.core.unknown_primitive_name_error" (func $hydra.decode.error.core.unknown_primitive_name_error) )
  (export "hydra.decode.error.core.unnecessary_identity_application_error" (func $hydra.decode.error.core.unnecessary_identity_application_error) )
  (export "hydra.decode.error.core.untyped_term_variable_error" (func $hydra.decode.error.core.untyped_term_variable_error) )
  (export "hydra.decode.error.core.void_in_non_bottom_position_error" (func $hydra.decode.error.core.void_in_non_bottom_position_error) )
  (func $hydra.decode.error.core.constant_condition_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $cx2 i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_value i32)
  (local $raw2 i32)
  (local $record i32)
  (local $stripped i32)
  (local $stripped2 i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 2197
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
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
  (block $boolean
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $boolean $boolean
)
  local.get $v
  drop
  i32.const 1
  local.get $b
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_value
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.duplicate_binding_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1644
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.duplicate_field_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1644
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.duplicate_record_type_field_names_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1644
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.duplicate_union_type_field_names_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1644
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.empty_case_statement_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_type_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1865
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_type_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.empty_let_bindings_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
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
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.empty_record_type_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
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
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.empty_term_annotation_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
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
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.empty_type_annotation_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
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
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.empty_type_name_in_term_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
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
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.empty_union_type_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
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
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.invalid_forall_parameter_name_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1644
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.invalid_lambda_parameter_name_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1644
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.invalid_let_binding_name_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1644
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.invalid_term_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $f i32)
  (local $field i32)
  (local $fname i32)
  (local $fterm i32)
  (local $inj i32)
  (local $input i32)
  (local $stripped i32)
  (local $t i32)
  (local $v i32)
  (local $variant_map i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $inject
  local.get $stripped
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
  i32.load offset=4
  local.set $field
  local.get $field
  i32.load
  local.set $fname
  local.get $field
  i32.load offset=4
  local.set $fterm
  i32.const 1065
  i32.const 0
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.constant_condition_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1086
  i32.const 1
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.duplicate_binding_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1106
  i32.const 2
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.duplicate_field_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1204
  i32.const 3
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.empty_case_statement_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1226
  i32.const 4
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.empty_let_bindings_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1265
  i32.const 5
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.empty_term_annotation_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1311
  i32.const 6
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.empty_type_name_in_term_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1499
  i32.const 7
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.invalid_lambda_parameter_name_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1529
  i32.const 8
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.invalid_let_binding_name_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1554
  i32.const 9
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.invalid_type_lambda_parameter_name_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1652
  i32.const 10
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.nested_term_annotation_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1776
  i32.const 11
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.redundant_wrap_unwrap_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1799
  i32.const 12
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.self_application_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1840
  i32.const 13
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.term_variable_shadowing_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1910
  i32.const 14
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.type_variable_shadowing_in_type_lambda_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1947
  i32.const 15
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.undefined_term_variable_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1997
  i32.const 16
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.undefined_type_variable_in_binding_type_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 2035
  i32.const 17
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.undefined_type_variable_in_lambda_domain_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 2074
  i32.const 18
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.undefined_type_variable_in_type_application_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 2116
  i32.const 19
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.unknown_primitive_name_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 2140
  i32.const 20
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.unnecessary_identity_application_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 2174
  i32.const 21
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.untyped_term_variable_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 92
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 22
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=88
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=84
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=80
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=76
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=72
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=68
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=64
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=60
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=56
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=52
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=48
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=44
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=40
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=36
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
  call $hydra.lib.maps.from_list
  local.set $variant_map
  i32.const 0
  i32.const 1700
  local.get $fname
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $fterm
  drop
  local.get $f
  drop
  i32.const 0
  local.get $fname
  local.get $variant_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.invalid_type_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $f i32)
  (local $field i32)
  (local $fname i32)
  (local $fterm i32)
  (local $inj i32)
  (local $input i32)
  (local $stripped i32)
  (local $t i32)
  (local $v i32)
  (local $variant_map i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $inject
  local.get $stripped
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
  i32.load offset=4
  local.set $field
  local.get $field
  i32.load
  local.set $fname
  local.get $field
  i32.load offset=4
  local.set $fterm
  i32.const 1124
  i32.const 0
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.duplicate_record_type_field_names_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1157
  i32.const 1
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.duplicate_union_type_field_names_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1246
  i32.const 2
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.empty_record_type_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1288
  i32.const 3
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.empty_type_annotation_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1334
  i32.const 4
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.empty_union_type_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1469
  i32.const 5
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.invalid_forall_parameter_name_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1588
  i32.const 6
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.invalid_type_scheme_variable_name_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1676
  i32.const 7
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.nested_type_annotation_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1718
  i32.const 8
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.non_comparable_map_key_type_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1745
  i32.const 9
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.non_comparable_set_element_type_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1818
  i32.const 10
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.single_variant_union_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1877
  i32.const 11
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.type_variable_shadowing_in_forall_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 1972
  i32.const 12
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.undefined_type_variable_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 2206
  i32.const 13
  local.get $t
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $input
  call $hydra.decode.error.core.void_in_non_bottom_position_error
  call $hydra.lib.eithers.map
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  i32.const 60
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 14
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=56
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=52
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=48
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=44
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=40
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=36
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
  call $hydra.lib.maps.from_list
  local.set $variant_map
  i32.const 0
  i32.const 1700
  local.get $fname
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $fterm
  drop
  local.get $f
  drop
  i32.const 0
  local.get $fname
  local.get $variant_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.invalid_type_lambda_parameter_name_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1644
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.invalid_type_scheme_variable_name_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1644
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.nested_term_annotation_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
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
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.nested_type_annotation_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
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
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.non_comparable_map_key_type_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_key_type i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1621
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_key_type
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.non_comparable_set_element_type_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_element_type i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1189
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_element_type
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.redundant_wrap_unwrap_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_type_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1865
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_type_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.self_application_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1644
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.single_variant_union_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_field_name i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1456
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_field_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.term_variable_shadowing_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1644
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.type_variable_shadowing_in_forall_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1644
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.type_variable_shadowing_in_type_lambda_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1644
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.undefined_field_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_field_name i32)
  (local $field_map i32)
  (local $field_type_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1456
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1865
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_field_name
  local.get $field_type_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.undefined_term_variable_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1644
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.undefined_type_variable_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1644
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.undefined_type_variable_in_binding_type_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1644
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.undefined_type_variable_in_lambda_domain_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1644
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.undefined_type_variable_in_type_application_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1644
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.unexpected_term_variant_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_actual_term i32)
  (local $field_expected_variant i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1437
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1037
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_expected_variant
  local.get $field_actual_term
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.unexpected_type_variant_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_actual_type i32)
  (local $field_expected_variant i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1437
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1051
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_expected_variant
  local.get $field_actual_type
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.unknown_primitive_name_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1644
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.unnecessary_identity_application_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
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
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.untyped_term_variable_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1644
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.void_in_non_bottom_position_error (param $cx i32) (param $raw i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  (local $v i32)
  i32.const 0
  local.get $err
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 1632
  i32.const 0
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
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
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.extract.core.strip_with_decoding_error
  call $hydra.lib.eithers.either
)
)
