(module
  (import "hydra.decode.core" "hydra.decode.core.name" (func $hydra.decode.core.name (param i32) (result i32) ) )
  (import "hydra.decode.core" "hydra.decode.core.term" (func $hydra.decode.core.term (param i32) (result i32) ) )
  (import "hydra.decode.core" "hydra.decode.core.type" (func $hydra.decode.core.type (param i32) (result i32) ) )
  (import "hydra.decode.paths" "hydra.decode.paths.subterm_path" (func $hydra.decode.paths.subterm_path (param i32) (result i32) ) )
  (import "hydra.decode.variants" "hydra.decode.variants.term_variant" (func $hydra.decode.variants.term_variant (param i32) (result i32) ) )
  (import "hydra.decode.variants" "hydra.decode.variants.type_variant" (func $hydra.decode.variants.type_variant (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.require_field" (func $hydra.extract.core.require_field (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.to_field_map" (func $hydra.extract.core.to_field_map (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.strip_and_dereference_term_either" (func $hydra.lexical.strip_and_dereference_term_either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
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
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "value"
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $boolean
  local.get $v
  br_table $boolean $boolean
)
  i32.const 1
  local.get $b
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.duplicate_binding_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.duplicate_field_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.duplicate_record_type_field_names_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.duplicate_union_type_field_names_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.empty_case_statement_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_type_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "typeName"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_type_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.empty_let_bindings_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.empty_record_type_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.empty_term_annotation_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.empty_type_annotation_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.empty_type_name_in_term_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.empty_union_type_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.invalid_forall_parameter_name_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.invalid_lambda_parameter_name_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.invalid_let_binding_name_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.invalid_term_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $f i32)
  (local $field i32)
  (local $fname i32)
  (local $fterm i32)
  (local $inj i32)
  (local $input i32)
  (local $stripped i32)
  (local $t i32)
  (local $variant_map i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $union
  local.get $stripped
  br_table $union $union
)
  local.get $inj
  ;; project field: field
  local.set $field
  local.get $field
  ;; project field: name
  local.set $fname
  local.get $field
  ;; project field: term
  local.set $fterm
  i32.const 22
  ;; list elements follow
  i32.const 0 ;; string: "constantCondition"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.constant_condition_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "duplicateBinding"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.duplicate_binding_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "duplicateField"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.duplicate_field_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "emptyCaseStatement"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.empty_case_statement_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "emptyLetBindings"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.empty_let_bindings_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "emptyTermAnnotation"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.empty_term_annotation_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "emptyTypeNameInTerm"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.empty_type_name_in_term_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "invalidLambdaParameterName"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.invalid_lambda_parameter_name_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "invalidLetBindingName"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.invalid_let_binding_name_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "invalidTypeLambdaParameterName"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.invalid_type_lambda_parameter_name_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "nestedTermAnnotation"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.nested_term_annotation_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "redundantWrapUnwrap"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.redundant_wrap_unwrap_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "selfApplication"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.self_application_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "termVariableShadowing"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.term_variable_shadowing_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "typeVariableShadowingInTypeLambda"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.type_variable_shadowing_in_type_lambda_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "undefinedTermVariable"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.undefined_term_variable_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "undefinedTypeVariableInBindingType"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.undefined_type_variable_in_binding_type_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "undefinedTypeVariableInLambdaDomain"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.undefined_type_variable_in_lambda_domain_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "undefinedTypeVariableInTypeApplication"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.undefined_type_variable_in_type_application_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "unknownPrimitiveName"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.unknown_primitive_name_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "unnecessaryIdentityApplication"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.unnecessary_identity_application_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "untypedTermVariable"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.untyped_term_variable_error
  call $hydra.lib.eithers.map
  call $hydra.lib.maps.from_list
  local.set $variant_map
  i32.const 0
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "no such field "
  nop
  i32.const 0 ;; string: " in union"
  call $hydra.lib.strings.cat
  local.get $fterm
  local.get $f
  local.get $fname
  local.get $variant_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.invalid_type_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $f i32)
  (local $field i32)
  (local $fname i32)
  (local $fterm i32)
  (local $inj i32)
  (local $input i32)
  (local $stripped i32)
  (local $t i32)
  (local $variant_map i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $union
  local.get $stripped
  br_table $union $union
)
  local.get $inj
  ;; project field: field
  local.set $field
  local.get $field
  ;; project field: name
  local.set $fname
  local.get $field
  ;; project field: term
  local.set $fterm
  i32.const 14
  ;; list elements follow
  i32.const 0 ;; string: "duplicateRecordTypeFieldNames"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.duplicate_record_type_field_names_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "duplicateUnionTypeFieldNames"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.duplicate_union_type_field_names_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "emptyRecordType"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.empty_record_type_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "emptyTypeAnnotation"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.empty_type_annotation_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "emptyUnionType"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.empty_union_type_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "invalidForallParameterName"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.invalid_forall_parameter_name_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "invalidTypeSchemeVariableName"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.invalid_type_scheme_variable_name_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "nestedTypeAnnotation"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.nested_type_annotation_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "nonComparableMapKeyType"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.non_comparable_map_key_type_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "nonComparableSetElementType"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.non_comparable_set_element_type_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "singleVariantUnion"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.single_variant_union_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "typeVariableShadowingInForall"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.type_variable_shadowing_in_forall_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "undefinedTypeVariable"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.undefined_type_variable_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "voidInNonBottomPosition"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.void_in_non_bottom_position_error
  call $hydra.lib.eithers.map
  call $hydra.lib.maps.from_list
  local.set $variant_map
  i32.const 0
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "no such field "
  nop
  i32.const 0 ;; string: " in union"
  call $hydra.lib.strings.cat
  local.get $fterm
  local.get $f
  local.get $fname
  local.get $variant_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.invalid_type_lambda_parameter_name_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.invalid_type_scheme_variable_name_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.nested_term_annotation_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.nested_type_annotation_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.non_comparable_map_key_type_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_key_type i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "keyType"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_key_type
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.non_comparable_set_element_type_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_element_type i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "elementType"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_element_type
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.redundant_wrap_unwrap_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_type_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "typeName"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_type_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.self_application_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.single_variant_union_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_field_name i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "fieldName"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_field_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.term_variable_shadowing_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.type_variable_shadowing_in_forall_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.type_variable_shadowing_in_type_lambda_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.undefined_field_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_field_name i32)
  (local $field_map i32)
  (local $field_type_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "fieldName"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "typeName"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_field_name
  local.get $field_type_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.undefined_term_variable_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.undefined_type_variable_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.undefined_type_variable_in_binding_type_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.undefined_type_variable_in_lambda_domain_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.undefined_type_variable_in_type_application_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.unexpected_term_variant_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_actual_term i32)
  (local $field_expected_variant i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "expectedVariant"
  call $hydra.decode.variants.term_variant
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "actualTerm"
  call $hydra.decode.core.term
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_expected_variant
  local.get $field_actual_term
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.unexpected_type_variant_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_actual_type i32)
  (local $field_expected_variant i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "expectedVariant"
  call $hydra.decode.variants.type_variant
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "actualType"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_expected_variant
  local.get $field_actual_type
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.unknown_primitive_name_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.unnecessary_identity_application_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.untyped_term_variable_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  local.get $field_name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.core.void_in_non_bottom_position_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_location i32)
  (local $field_map i32)
  (local $record i32)
  (local $stripped i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  local.get $record
  call $hydra.extract.core.to_field_map
  local.set $field_map
  i32.const 0 ;; string: "location"
  call $hydra.decode.paths.subterm_path
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_location
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
)
