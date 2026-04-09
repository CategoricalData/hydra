(module
  (import "hydra.lib.literals" "hydra.lib.literals.show_boolean" (func $hydra.lib.literals.show_boolean (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.term" (func $hydra.show.core.term (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.type" (func $hydra.show.core.type (param i32) (result i32) ) )
  (import "hydra.show.variants" "hydra.show.variants.term_variant" (func $hydra.show.variants.term_variant (param i32) (result i32) ) )
  (import "hydra.show.variants" "hydra.show.variants.type_variant" (func $hydra.show.variants.type_variant (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
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
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "constant condition: ifElse with literal "
  local.get $e
  ;; project field: value
  call $hydra.lib.literals.show_boolean
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.duplicate_binding_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "duplicate binding: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.duplicate_field_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "duplicate field: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.duplicate_record_type_field_names_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "duplicate field in record type: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.duplicate_union_type_field_names_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "duplicate field in union type: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.empty_case_statement_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "empty case statement for type: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.empty_let_bindings_error (param $e i32) (result i32)
  i32.const 0 ;; string: "let expression with no bindings"
)
  (func $hydra.show.error.core.empty_record_type_error (param $e i32) (result i32)
  i32.const 0 ;; string: "record type with no fields (use TypeUnit instead)"
)
  (func $hydra.show.error.core.empty_term_annotation_error (param $e i32) (result i32)
  i32.const 0 ;; string: "term annotation with empty annotation map"
)
  (func $hydra.show.error.core.empty_type_annotation_error (param $e i32) (result i32)
  i32.const 0 ;; string: "type annotation with empty annotation map"
)
  (func $hydra.show.error.core.empty_type_name_in_term_error (param $e i32) (result i32)
  i32.const 0 ;; string: "term with empty type name"
)
  (func $hydra.show.error.core.empty_union_type_error (param $e i32) (result i32)
  i32.const 0 ;; string: "union type with no alternatives (use TypeVoid instead)"
)
  (func $hydra.show.error.core.invalid_forall_parameter_name_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "invalid forall parameter name: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.invalid_lambda_parameter_name_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "invalid lambda parameter name: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.invalid_let_binding_name_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "invalid let binding name: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.invalid_term_error (param $e i32) (result i32)
  (local $v i32)
  i32.const 0 ;; string: "invalid term: "
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
  (local $v i32)
  i32.const 0 ;; string: "invalid type: "
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
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "invalid type lambda parameter name: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.invalid_type_scheme_variable_name_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "invalid type scheme variable name: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.nested_term_annotation_error (param $e i32) (result i32)
  i32.const 0 ;; string: "nested term annotations should be merged"
)
  (func $hydra.show.error.core.nested_type_annotation_error (param $e i32) (result i32)
  i32.const 0 ;; string: "nested type annotations should be merged"
)
  (func $hydra.show.error.core.non_comparable_map_key_type_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "map key type contains a function type: "
  local.get $e
  ;; project field: key_type
  call $hydra.show.core.type
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.non_comparable_set_element_type_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "set element type contains a function type: "
  local.get $e
  ;; project field: element_type
  call $hydra.show.core.type
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.redundant_wrap_unwrap_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "redundant wrap/unwrap for type: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.self_application_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "self-application of variable: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.single_variant_union_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "union type with single variant: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.term_variable_shadowing_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "variable shadowing: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.type_variable_shadowing_in_forall_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "type variable shadowing in forall: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.type_variable_shadowing_in_type_lambda_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "type variable shadowing in type lambda: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.undefined_field_error (param $e i32) (result i32)
  (local $fname i32)
  (local $tname i32)
  local.get $e
  ;; project field: field_name
  local.set $fname
  local.get $e
  ;; project field: type_name
  local.set $tname
  i32.const 5
  ;; list elements follow
  i32.const 0 ;; string: "no such field ""
  nop
  i32.const 0 ;; string: "" in type ""
  nop
  i32.const 0 ;; string: """
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.undefined_term_variable_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "undefined term variable: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.undefined_type_variable_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "undefined type variable: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.undefined_type_variable_in_binding_type_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "undefined type variable in binding type: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.undefined_type_variable_in_lambda_domain_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "undefined type variable in lambda domain: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.undefined_type_variable_in_type_application_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "undefined type variable in type application: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.unexpected_term_variant_error (param $e i32) (result i32)
  (local $actual i32)
  (local $expected i32)
  local.get $e
  ;; project field: expected_variant
  local.set $expected
  local.get $e
  ;; project field: actual_term
  local.set $actual
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  local.get $expected
  call $hydra.show.variants.term_variant
  i32.const 0 ;; string: " term but found "
  local.get $actual
  call $hydra.show.core.term
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.unexpected_type_variant_error (param $e i32) (result i32)
  (local $actual i32)
  (local $expected i32)
  local.get $e
  ;; project field: expected_variant
  local.set $expected
  local.get $e
  ;; project field: actual_type
  local.set $actual
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "expected "
  local.get $expected
  call $hydra.show.variants.type_variant
  i32.const 0 ;; string: " type but found "
  local.get $actual
  call $hydra.show.core.type
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.unknown_primitive_name_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "unknown primitive: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.unnecessary_identity_application_error (param $e i32) (result i32)
  i32.const 0 ;; string: "unnecessary application of identity lambda"
)
  (func $hydra.show.error.core.untyped_term_variable_error (param $e i32) (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "untyped term variable: "
  nop
  call $hydra.lib.strings.cat
)
  (func $hydra.show.error.core.void_in_non_bottom_position_error (param $e i32) (result i32)
  i32.const 0 ;; string: "TypeVoid in a position where no value can be constructed"
)
)
