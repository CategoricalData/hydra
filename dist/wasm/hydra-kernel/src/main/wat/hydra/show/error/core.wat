(module
  (import "hydra.lib.literals" "hydra.lib.literals.show_boolean" (func $hydra.lib.literals.show_boolean (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.term" (func $hydra.show.core.term (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.type" (func $hydra.show.core.type (param i32) (result i32) ) )
  (import "hydra.show.variants" "hydra.show.variants.term_variant" (func $hydra.show.variants.term_variant (param i32) (result i32) ) )
  (import "hydra.show.variants" "hydra.show.variants.type_variant" (func $hydra.show.variants.type_variant (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\10\00\00\00\20\74\65\72\6d\20\62\75\74\20\66\6f\75\6e\64\20\10\00\00\00\20\74\79\70\65\20\62\75\74\20\66\6f\75\6e\64\20\01\00\00\00\22\0b\00\00\00\22\20\69\6e\20\74\79\70\65\20\22\38\00\00\00\54\79\70\65\56\6f\69\64\20\69\6e\20\61\20\70\6f\73\69\74\69\6f\6e\20\77\68\65\72\65\20\6e\6f\20\76\61\6c\75\65\20\63\61\6e\20\62\65\20\63\6f\6e\73\74\72\75\63\74\65\64\28\00\00\00\63\6f\6e\73\74\61\6e\74\20\63\6f\6e\64\69\74\69\6f\6e\3a\20\69\66\45\6c\73\65\20\77\69\74\68\20\6c\69\74\65\72\61\6c\20\13\00\00\00\64\75\70\6c\69\63\61\74\65\20\62\69\6e\64\69\6e\67\3a\20\20\00\00\00\64\75\70\6c\69\63\61\74\65\20\66\69\65\6c\64\20\69\6e\20\72\65\63\6f\72\64\20\74\79\70\65\3a\20\1f\00\00\00\64\75\70\6c\69\63\61\74\65\20\66\69\65\6c\64\20\69\6e\20\75\6e\69\6f\6e\20\74\79\70\65\3a\20\11\00\00\00\64\75\70\6c\69\63\61\74\65\20\66\69\65\6c\64\3a\20\1f\00\00\00\65\6d\70\74\79\20\63\61\73\65\20\73\74\61\74\65\6d\65\6e\74\20\66\6f\72\20\74\79\70\65\3a\20\09\00\00\00\65\78\70\65\63\74\65\64\20\1f\00\00\00\69\6e\76\61\6c\69\64\20\66\6f\72\61\6c\6c\20\70\61\72\61\6d\65\74\65\72\20\6e\61\6d\65\3a\20\1f\00\00\00\69\6e\76\61\6c\69\64\20\6c\61\6d\62\64\61\20\70\61\72\61\6d\65\74\65\72\20\6e\61\6d\65\3a\20\1a\00\00\00\69\6e\76\61\6c\69\64\20\6c\65\74\20\62\69\6e\64\69\6e\67\20\6e\61\6d\65\3a\20\0e\00\00\00\69\6e\76\61\6c\69\64\20\74\65\72\6d\3a\20\24\00\00\00\69\6e\76\61\6c\69\64\20\74\79\70\65\20\6c\61\6d\62\64\61\20\70\61\72\61\6d\65\74\65\72\20\6e\61\6d\65\3a\20\23\00\00\00\69\6e\76\61\6c\69\64\20\74\79\70\65\20\73\63\68\65\6d\65\20\76\61\72\69\61\62\6c\65\20\6e\61\6d\65\3a\20\0e\00\00\00\69\6e\76\61\6c\69\64\20\74\79\70\65\3a\20\1f\00\00\00\6c\65\74\20\65\78\70\72\65\73\73\69\6f\6e\20\77\69\74\68\20\6e\6f\20\62\69\6e\64\69\6e\67\73\27\00\00\00\6d\61\70\20\6b\65\79\20\74\79\70\65\20\63\6f\6e\74\61\69\6e\73\20\61\20\66\75\6e\63\74\69\6f\6e\20\74\79\70\65\3a\20\28\00\00\00\6e\65\73\74\65\64\20\74\65\72\6d\20\61\6e\6e\6f\74\61\74\69\6f\6e\73\20\73\68\6f\75\6c\64\20\62\65\20\6d\65\72\67\65\64\28\00\00\00\6e\65\73\74\65\64\20\74\79\70\65\20\61\6e\6e\6f\74\61\74\69\6f\6e\73\20\73\68\6f\75\6c\64\20\62\65\20\6d\65\72\67\65\64\0f\00\00\00\6e\6f\20\73\75\63\68\20\66\69\65\6c\64\20\22\31\00\00\00\72\65\63\6f\72\64\20\74\79\70\65\20\77\69\74\68\20\6e\6f\20\66\69\65\6c\64\73\20\28\75\73\65\20\54\79\70\65\55\6e\69\74\20\69\6e\73\74\65\61\64\29\20\00\00\00\72\65\64\75\6e\64\61\6e\74\20\77\72\61\70\2f\75\6e\77\72\61\70\20\66\6f\72\20\74\79\70\65\3a\20\1e\00\00\00\73\65\6c\66\2d\61\70\70\6c\69\63\61\74\69\6f\6e\20\6f\66\20\76\61\72\69\61\62\6c\65\3a\20\2b\00\00\00\73\65\74\20\65\6c\65\6d\65\6e\74\20\74\79\70\65\20\63\6f\6e\74\61\69\6e\73\20\61\20\66\75\6e\63\74\69\6f\6e\20\74\79\70\65\3a\20\29\00\00\00\74\65\72\6d\20\61\6e\6e\6f\74\61\74\69\6f\6e\20\77\69\74\68\20\65\6d\70\74\79\20\61\6e\6e\6f\74\61\74\69\6f\6e\20\6d\61\70\19\00\00\00\74\65\72\6d\20\77\69\74\68\20\65\6d\70\74\79\20\74\79\70\65\20\6e\61\6d\65\29\00\00\00\74\79\70\65\20\61\6e\6e\6f\74\61\74\69\6f\6e\20\77\69\74\68\20\65\6d\70\74\79\20\61\6e\6e\6f\74\61\74\69\6f\6e\20\6d\61\70\23\00\00\00\74\79\70\65\20\76\61\72\69\61\62\6c\65\20\73\68\61\64\6f\77\69\6e\67\20\69\6e\20\66\6f\72\61\6c\6c\3a\20\28\00\00\00\74\79\70\65\20\76\61\72\69\61\62\6c\65\20\73\68\61\64\6f\77\69\6e\67\20\69\6e\20\74\79\70\65\20\6c\61\6d\62\64\61\3a\20\19\00\00\00\75\6e\64\65\66\69\6e\65\64\20\74\65\72\6d\20\76\61\72\69\61\62\6c\65\3a\20\29\00\00\00\75\6e\64\65\66\69\6e\65\64\20\74\79\70\65\20\76\61\72\69\61\62\6c\65\20\69\6e\20\62\69\6e\64\69\6e\67\20\74\79\70\65\3a\20\2a\00\00\00\75\6e\64\65\66\69\6e\65\64\20\74\79\70\65\20\76\61\72\69\61\62\6c\65\20\69\6e\20\6c\61\6d\62\64\61\20\64\6f\6d\61\69\6e\3a\20\2d\00\00\00\75\6e\64\65\66\69\6e\65\64\20\74\79\70\65\20\76\61\72\69\61\62\6c\65\20\69\6e\20\74\79\70\65\20\61\70\70\6c\69\63\61\74\69\6f\6e\3a\20\19\00\00\00\75\6e\64\65\66\69\6e\65\64\20\74\79\70\65\20\76\61\72\69\61\62\6c\65\3a\20\36\00\00\00\75\6e\69\6f\6e\20\74\79\70\65\20\77\69\74\68\20\6e\6f\20\61\6c\74\65\72\6e\61\74\69\76\65\73\20\28\75\73\65\20\54\79\70\65\56\6f\69\64\20\69\6e\73\74\65\61\64\29\20\00\00\00\75\6e\69\6f\6e\20\74\79\70\65\20\77\69\74\68\20\73\69\6e\67\6c\65\20\76\61\72\69\61\6e\74\3a\20\13\00\00\00\75\6e\6b\6e\6f\77\6e\20\70\72\69\6d\69\74\69\76\65\3a\20\2a\00\00\00\75\6e\6e\65\63\65\73\73\61\72\79\20\61\70\70\6c\69\63\61\74\69\6f\6e\20\6f\66\20\69\64\65\6e\74\69\74\79\20\6c\61\6d\62\64\61\17\00\00\00\75\6e\74\79\70\65\64\20\74\65\72\6d\20\76\61\72\69\61\62\6c\65\3a\20\14\00\00\00\76\61\72\69\61\62\6c\65\20\73\68\61\64\6f\77\69\6e\67\3a\20")
  (global $__bump_ptr (mut i32) i32.const 2544 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.show.error.core.constant_condition_error" (func $hydra.show.error.core.constant_condition_error) )
  (export "hydra.show.error.core.duplicate_binding_error" (func $hydra.show.error.core.duplicate_binding_error) )
  (export "hydra.show.error.core.duplicate_field_error" (func $hydra.show.error.core.duplicate_field_error) )
  (export "hydra.show.error.core.duplicate_record_type_field_names_error" (func $hydra.show.error.core.duplicate_record_type_field_names_error) )
  (export "hydra.show.error.core.duplicate_union_type_field_names_error" (func $hydra.show.error.core.duplicate_union_type_field_names_error) )
  (export "hydra.show.error.core.empty_case_statement_error" (func $hydra.show.error.core.empty_case_statement_error) )
  (export "hydra.show.error.core.empty_let_bindings_error" (func $hydra.show.error.core.empty_let_bindings_error) )
  (export "hydra.show.error.core.empty_record_type_error" (func $hydra.show.error.core.empty_record_type_error) )
  (export "hydra.show.error.core.empty_term_annotation_error" (func $hydra.show.error.core.empty_term_annotation_error) )
  (export "hydra.show.error.core.empty_type_annotation_error" (func $hydra.show.error.core.empty_type_annotation_error) )
  (export "hydra.show.error.core.empty_type_name_in_term_error" (func $hydra.show.error.core.empty_type_name_in_term_error) )
  (export "hydra.show.error.core.empty_union_type_error" (func $hydra.show.error.core.empty_union_type_error) )
  (export "hydra.show.error.core.invalid_forall_parameter_name_error" (func $hydra.show.error.core.invalid_forall_parameter_name_error) )
  (export "hydra.show.error.core.invalid_lambda_parameter_name_error" (func $hydra.show.error.core.invalid_lambda_parameter_name_error) )
  (export "hydra.show.error.core.invalid_let_binding_name_error" (func $hydra.show.error.core.invalid_let_binding_name_error) )
  (export "hydra.show.error.core.invalid_term_error" (func $hydra.show.error.core.invalid_term_error) )
  (export "hydra.show.error.core.invalid_type_error" (func $hydra.show.error.core.invalid_type_error) )
  (export "hydra.show.error.core.invalid_type_lambda_parameter_name_error" (func $hydra.show.error.core.invalid_type_lambda_parameter_name_error) )
  (export "hydra.show.error.core.invalid_type_scheme_variable_name_error" (func $hydra.show.error.core.invalid_type_scheme_variable_name_error) )
  (export "hydra.show.error.core.nested_term_annotation_error" (func $hydra.show.error.core.nested_term_annotation_error) )
  (export "hydra.show.error.core.nested_type_annotation_error" (func $hydra.show.error.core.nested_type_annotation_error) )
  (export "hydra.show.error.core.non_comparable_map_key_type_error" (func $hydra.show.error.core.non_comparable_map_key_type_error) )
  (export "hydra.show.error.core.non_comparable_set_element_type_error" (func $hydra.show.error.core.non_comparable_set_element_type_error) )
  (export "hydra.show.error.core.redundant_wrap_unwrap_error" (func $hydra.show.error.core.redundant_wrap_unwrap_error) )
  (export "hydra.show.error.core.self_application_error" (func $hydra.show.error.core.self_application_error) )
  (export "hydra.show.error.core.single_variant_union_error" (func $hydra.show.error.core.single_variant_union_error) )
  (export "hydra.show.error.core.term_variable_shadowing_error" (func $hydra.show.error.core.term_variable_shadowing_error) )
  (export "hydra.show.error.core.type_variable_shadowing_in_forall_error" (func $hydra.show.error.core.type_variable_shadowing_in_forall_error) )
  (export "hydra.show.error.core.type_variable_shadowing_in_type_lambda_error" (func $hydra.show.error.core.type_variable_shadowing_in_type_lambda_error) )
  (export "hydra.show.error.core.undefined_field_error" (func $hydra.show.error.core.undefined_field_error) )
  (export "hydra.show.error.core.undefined_term_variable_error" (func $hydra.show.error.core.undefined_term_variable_error) )
  (export "hydra.show.error.core.undefined_type_variable_error" (func $hydra.show.error.core.undefined_type_variable_error) )
  (export "hydra.show.error.core.undefined_type_variable_in_binding_type_error" (func $hydra.show.error.core.undefined_type_variable_in_binding_type_error) )
  (export "hydra.show.error.core.undefined_type_variable_in_lambda_domain_error" (func $hydra.show.error.core.undefined_type_variable_in_lambda_domain_error) )
  (export "hydra.show.error.core.undefined_type_variable_in_type_application_error" (func $hydra.show.error.core.undefined_type_variable_in_type_application_error) )
  (export "hydra.show.error.core.unexpected_term_variant_error" (func $hydra.show.error.core.unexpected_term_variant_error) )
  (export "hydra.show.error.core.unexpected_type_variant_error" (func $hydra.show.error.core.unexpected_type_variant_error) )
  (export "hydra.show.error.core.unknown_primitive_name_error" (func $hydra.show.error.core.unknown_primitive_name_error) )
  (export "hydra.show.error.core.unnecessary_identity_application_error" (func $hydra.show.error.core.unnecessary_identity_application_error) )
  (export "hydra.show.error.core.untyped_term_variable_error" (func $hydra.show.error.core.untyped_term_variable_error) )
  (export "hydra.show.error.core.void_in_non_bottom_position_error" (func $hydra.show.error.core.void_in_non_bottom_position_error) )
  (func $hydra.show.error.core.constant_condition_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1144
  local.get $e
  i32.load offset=4
  call $hydra.lib.literals.show_boolean
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.duplicate_binding_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1188
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.duplicate_field_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1282
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.duplicate_record_type_field_names_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1211
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.duplicate_union_type_field_names_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1247
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.empty_case_statement_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1303
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.empty_let_bindings_error (param $e i32) (result i32)
  i32.const 1566
)
  (func $hydra.show.error.core.empty_record_type_error (param $e i32) (result i32)
  i32.const 1751
)
  (func $hydra.show.error.core.empty_term_annotation_error (param $e i32) (result i32)
  i32.const 1921
)
  (func $hydra.show.error.core.empty_type_annotation_error (param $e i32) (result i32)
  i32.const 1995
)
  (func $hydra.show.error.core.empty_type_name_in_term_error (param $e i32) (result i32)
  i32.const 1966
)
  (func $hydra.show.error.core.empty_union_type_error (param $e i32) (result i32)
  i32.const 2321
)
  (func $hydra.show.error.core.invalid_forall_parameter_name_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1351
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.invalid_lambda_parameter_name_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1386
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.invalid_let_binding_name_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1421
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.invalid_term_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $v i32)
  i32.const 1451
  (block $end_invalid_term_error (result i32)
  (block $untyped_term_variable
  (block $unnecessary_identity_application
  (block $unknown_primitive_name
  (block $undefined_type_variable_in_type_application
  (block $undefined_type_variable_in_lambda_domain
  (block $undefined_type_variable_in_binding_type
  (block $undefined_term_variable
  (block $type_variable_shadowing_in_type_lambda
  (block $term_variable_shadowing
  (block $self_application
  (block $redundant_wrap_unwrap
  (block $nested_term_annotation
  (block $invalid_type_lambda_parameter_name
  (block $invalid_let_binding_name
  (block $invalid_lambda_parameter_name
  (block $empty_type_name_in_term
  (block $empty_term_annotation
  (block $empty_let_bindings
  (block $empty_case_statement
  (block $duplicate_field
  (block $duplicate_binding
  (block $constant_condition
  local.get $e
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $constant_condition $duplicate_binding $duplicate_field $empty_case_statement $empty_let_bindings $empty_term_annotation $empty_type_name_in_term $invalid_lambda_parameter_name $invalid_let_binding_name $invalid_type_lambda_parameter_name $nested_term_annotation $redundant_wrap_unwrap $self_application $term_variable_shadowing $type_variable_shadowing_in_type_lambda $undefined_term_variable $undefined_type_variable_in_binding_type $undefined_type_variable_in_lambda_domain $undefined_type_variable_in_type_application $unknown_primitive_name $unnecessary_identity_application $untyped_term_variable $untyped_term_variable
)
  local.get $v
  call $hydra.show.error.core.constant_condition_error
  br $end_invalid_term_error
)
  local.get $v
  call $hydra.show.error.core.duplicate_binding_error
  br $end_invalid_term_error
)
  local.get $v
  call $hydra.show.error.core.duplicate_field_error
  br $end_invalid_term_error
)
  local.get $v
  call $hydra.show.error.core.empty_case_statement_error
  br $end_invalid_term_error
)
  local.get $v
  call $hydra.show.error.core.empty_let_bindings_error
  br $end_invalid_term_error
)
  local.get $v
  call $hydra.show.error.core.empty_term_annotation_error
  br $end_invalid_term_error
)
  local.get $v
  call $hydra.show.error.core.empty_type_name_in_term_error
  br $end_invalid_term_error
)
  local.get $v
  call $hydra.show.error.core.invalid_lambda_parameter_name_error
  br $end_invalid_term_error
)
  local.get $v
  call $hydra.show.error.core.invalid_let_binding_name_error
  br $end_invalid_term_error
)
  local.get $v
  call $hydra.show.error.core.invalid_type_lambda_parameter_name_error
  br $end_invalid_term_error
)
  local.get $v
  call $hydra.show.error.core.nested_term_annotation_error
  br $end_invalid_term_error
)
  local.get $v
  call $hydra.show.error.core.redundant_wrap_unwrap_error
  br $end_invalid_term_error
)
  local.get $v
  call $hydra.show.error.core.self_application_error
  br $end_invalid_term_error
)
  local.get $v
  call $hydra.show.error.core.term_variable_shadowing_error
  br $end_invalid_term_error
)
  local.get $v
  call $hydra.show.error.core.type_variable_shadowing_in_type_lambda_error
  br $end_invalid_term_error
)
  local.get $v
  call $hydra.show.error.core.undefined_term_variable_error
  br $end_invalid_term_error
)
  local.get $v
  call $hydra.show.error.core.undefined_type_variable_in_binding_type_error
  br $end_invalid_term_error
)
  local.get $v
  call $hydra.show.error.core.undefined_type_variable_in_lambda_domain_error
  br $end_invalid_term_error
)
  local.get $v
  call $hydra.show.error.core.undefined_type_variable_in_type_application_error
  br $end_invalid_term_error
)
  local.get $v
  call $hydra.show.error.core.unknown_primitive_name_error
  br $end_invalid_term_error
)
  local.get $v
  call $hydra.show.error.core.unnecessary_identity_application_error
  br $end_invalid_term_error
)
  local.get $v
  call $hydra.show.error.core.untyped_term_variable_error
  br $end_invalid_term_error
)
  call $hydra.lib.strings.cat2
)
  (func $hydra.show.error.core.invalid_type_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $v i32)
  i32.const 1548
  (block $end_invalid_type_error (result i32)
  (block $void_in_non_bottom_position
  (block $undefined_type_variable
  (block $type_variable_shadowing_in_forall
  (block $single_variant_union
  (block $non_comparable_set_element_type
  (block $non_comparable_map_key_type
  (block $nested_type_annotation
  (block $invalid_type_scheme_variable_name
  (block $invalid_forall_parameter_name
  (block $empty_union_type
  (block $empty_type_annotation
  (block $empty_record_type
  (block $duplicate_union_type_field_names
  (block $duplicate_record_type_field_names
  local.get $e
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $duplicate_record_type_field_names $duplicate_union_type_field_names $empty_record_type $empty_type_annotation $empty_union_type $invalid_forall_parameter_name $invalid_type_scheme_variable_name $nested_type_annotation $non_comparable_map_key_type $non_comparable_set_element_type $single_variant_union $type_variable_shadowing_in_forall $undefined_type_variable $void_in_non_bottom_position $void_in_non_bottom_position
)
  local.get $v
  call $hydra.show.error.core.duplicate_record_type_field_names_error
  br $end_invalid_type_error
)
  local.get $v
  call $hydra.show.error.core.duplicate_union_type_field_names_error
  br $end_invalid_type_error
)
  local.get $v
  call $hydra.show.error.core.empty_record_type_error
  br $end_invalid_type_error
)
  local.get $v
  call $hydra.show.error.core.empty_type_annotation_error
  br $end_invalid_type_error
)
  local.get $v
  call $hydra.show.error.core.empty_union_type_error
  br $end_invalid_type_error
)
  local.get $v
  call $hydra.show.error.core.invalid_forall_parameter_name_error
  br $end_invalid_type_error
)
  local.get $v
  call $hydra.show.error.core.invalid_type_scheme_variable_name_error
  br $end_invalid_type_error
)
  local.get $v
  call $hydra.show.error.core.nested_type_annotation_error
  br $end_invalid_type_error
)
  local.get $v
  call $hydra.show.error.core.non_comparable_map_key_type_error
  br $end_invalid_type_error
)
  local.get $v
  call $hydra.show.error.core.non_comparable_set_element_type_error
  br $end_invalid_type_error
)
  local.get $v
  call $hydra.show.error.core.single_variant_union_error
  br $end_invalid_type_error
)
  local.get $v
  call $hydra.show.error.core.type_variable_shadowing_in_forall_error
  br $end_invalid_type_error
)
  local.get $v
  call $hydra.show.error.core.undefined_type_variable_error
  br $end_invalid_type_error
)
  local.get $v
  call $hydra.show.error.core.void_in_non_bottom_position_error
  br $end_invalid_type_error
)
  call $hydra.lib.strings.cat2
)
  (func $hydra.show.error.core.invalid_type_lambda_parameter_name_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1469
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.invalid_type_scheme_variable_name_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1509
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.nested_term_annotation_error (param $e i32) (result i32)
  i32.const 1644
)
  (func $hydra.show.error.core.nested_type_annotation_error (param $e i32) (result i32)
  i32.const 1688
)
  (func $hydra.show.error.core.non_comparable_map_key_type_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1601
  local.get $e
  i32.load offset=4
  call $hydra.show.core.type
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.non_comparable_set_element_type_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1874
  local.get $e
  i32.load offset=4
  call $hydra.show.core.type
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.redundant_wrap_unwrap_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1804
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.self_application_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1840
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.single_variant_union_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 2379
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.term_variable_shadowing_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 2511
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.type_variable_shadowing_in_forall_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 2040
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.type_variable_shadowing_in_type_lambda_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 2079
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.undefined_field_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $fname i32)
  (local $tname i32)
  local.get $e
  i32.load
  local.set $fname
  local.get $e
  i32.load offset=4
  local.set $tname
  i32.const 1732
  local.get $fname
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1069
  local.get $tname
  drop
  i32.const 0
  drop
  i32.const 0
  i32.const 1064
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
)
  (func $hydra.show.error.core.undefined_term_variable_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 2123
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.undefined_type_variable_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 2292
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.undefined_type_variable_in_binding_type_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 2152
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.undefined_type_variable_in_lambda_domain_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 2197
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.undefined_type_variable_in_type_application_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 2243
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.unexpected_term_variant_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $actual i32)
  (local $expected i32)
  local.get $e
  i32.load
  local.set $expected
  local.get $e
  i32.load offset=4
  local.set $actual
  i32.const 1338
  local.get $expected
  call $hydra.show.variants.term_variant
  i32.const 1024
  local.get $actual
  call $hydra.show.core.term
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
  (func $hydra.show.error.core.unexpected_type_variant_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $actual i32)
  (local $expected i32)
  local.get $e
  i32.load
  local.set $expected
  local.get $e
  i32.load offset=4
  local.set $actual
  i32.const 1338
  local.get $expected
  call $hydra.show.variants.type_variant
  i32.const 1044
  local.get $actual
  call $hydra.show.core.type
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
  (func $hydra.show.error.core.unknown_primitive_name_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 2415
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.unnecessary_identity_application_error (param $e i32) (result i32)
  i32.const 2438
)
  (func $hydra.show.error.core.untyped_term_variable_error (param $e i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 2484
  local.get $e
  i32.load offset=4
  drop
  i32.const 0
  drop
  i32.const 0
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
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.void_in_non_bottom_position_error (param $e i32) (result i32)
  i32.const 1084
)
)
