(module
  (import "hydra.encode.core" "hydra.encode.core.name" (func $hydra.encode.core.name (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.term" (func $hydra.encode.core.term (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.type" (func $hydra.encode.core.type (param i32) (result i32) ) )
  (import "hydra.encode.paths" "hydra.encode.paths.subterm_path" (func $hydra.encode.paths.subterm_path (param i32) (result i32) ) )
  (import "hydra.encode.variants" "hydra.encode.variants.term_variant" (func $hydra.encode.variants.term_variant (param i32) (result i32) ) )
  (import "hydra.encode.variants" "hydra.encode.variants.type_variant" (func $hydra.encode.variants.type_variant (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.encode.error.core.constant_condition_error" (func $hydra.encode.error.core.constant_condition_error) )
  (export "hydra.encode.error.core.duplicate_binding_error" (func $hydra.encode.error.core.duplicate_binding_error) )
  (export "hydra.encode.error.core.duplicate_field_error" (func $hydra.encode.error.core.duplicate_field_error) )
  (export "hydra.encode.error.core.duplicate_record_type_field_names_error" (func $hydra.encode.error.core.duplicate_record_type_field_names_error) )
  (export "hydra.encode.error.core.duplicate_union_type_field_names_error" (func $hydra.encode.error.core.duplicate_union_type_field_names_error) )
  (export "hydra.encode.error.core.empty_case_statement_error" (func $hydra.encode.error.core.empty_case_statement_error) )
  (export "hydra.encode.error.core.empty_let_bindings_error" (func $hydra.encode.error.core.empty_let_bindings_error) )
  (export "hydra.encode.error.core.empty_record_type_error" (func $hydra.encode.error.core.empty_record_type_error) )
  (export "hydra.encode.error.core.empty_term_annotation_error" (func $hydra.encode.error.core.empty_term_annotation_error) )
  (export "hydra.encode.error.core.empty_type_annotation_error" (func $hydra.encode.error.core.empty_type_annotation_error) )
  (export "hydra.encode.error.core.empty_type_name_in_term_error" (func $hydra.encode.error.core.empty_type_name_in_term_error) )
  (export "hydra.encode.error.core.empty_union_type_error" (func $hydra.encode.error.core.empty_union_type_error) )
  (export "hydra.encode.error.core.invalid_forall_parameter_name_error" (func $hydra.encode.error.core.invalid_forall_parameter_name_error) )
  (export "hydra.encode.error.core.invalid_lambda_parameter_name_error" (func $hydra.encode.error.core.invalid_lambda_parameter_name_error) )
  (export "hydra.encode.error.core.invalid_let_binding_name_error" (func $hydra.encode.error.core.invalid_let_binding_name_error) )
  (export "hydra.encode.error.core.invalid_term_error" (func $hydra.encode.error.core.invalid_term_error) )
  (export "hydra.encode.error.core.invalid_type_error" (func $hydra.encode.error.core.invalid_type_error) )
  (export "hydra.encode.error.core.invalid_type_lambda_parameter_name_error" (func $hydra.encode.error.core.invalid_type_lambda_parameter_name_error) )
  (export "hydra.encode.error.core.invalid_type_scheme_variable_name_error" (func $hydra.encode.error.core.invalid_type_scheme_variable_name_error) )
  (export "hydra.encode.error.core.nested_term_annotation_error" (func $hydra.encode.error.core.nested_term_annotation_error) )
  (export "hydra.encode.error.core.nested_type_annotation_error" (func $hydra.encode.error.core.nested_type_annotation_error) )
  (export "hydra.encode.error.core.non_comparable_map_key_type_error" (func $hydra.encode.error.core.non_comparable_map_key_type_error) )
  (export "hydra.encode.error.core.non_comparable_set_element_type_error" (func $hydra.encode.error.core.non_comparable_set_element_type_error) )
  (export "hydra.encode.error.core.redundant_wrap_unwrap_error" (func $hydra.encode.error.core.redundant_wrap_unwrap_error) )
  (export "hydra.encode.error.core.self_application_error" (func $hydra.encode.error.core.self_application_error) )
  (export "hydra.encode.error.core.single_variant_union_error" (func $hydra.encode.error.core.single_variant_union_error) )
  (export "hydra.encode.error.core.term_variable_shadowing_error" (func $hydra.encode.error.core.term_variable_shadowing_error) )
  (export "hydra.encode.error.core.type_variable_shadowing_in_forall_error" (func $hydra.encode.error.core.type_variable_shadowing_in_forall_error) )
  (export "hydra.encode.error.core.type_variable_shadowing_in_type_lambda_error" (func $hydra.encode.error.core.type_variable_shadowing_in_type_lambda_error) )
  (export "hydra.encode.error.core.undefined_field_error" (func $hydra.encode.error.core.undefined_field_error) )
  (export "hydra.encode.error.core.undefined_term_variable_error" (func $hydra.encode.error.core.undefined_term_variable_error) )
  (export "hydra.encode.error.core.undefined_type_variable_error" (func $hydra.encode.error.core.undefined_type_variable_error) )
  (export "hydra.encode.error.core.undefined_type_variable_in_binding_type_error" (func $hydra.encode.error.core.undefined_type_variable_in_binding_type_error) )
  (export "hydra.encode.error.core.undefined_type_variable_in_lambda_domain_error" (func $hydra.encode.error.core.undefined_type_variable_in_lambda_domain_error) )
  (export "hydra.encode.error.core.undefined_type_variable_in_type_application_error" (func $hydra.encode.error.core.undefined_type_variable_in_type_application_error) )
  (export "hydra.encode.error.core.unexpected_term_variant_error" (func $hydra.encode.error.core.unexpected_term_variant_error) )
  (export "hydra.encode.error.core.unexpected_type_variant_error" (func $hydra.encode.error.core.unexpected_type_variant_error) )
  (export "hydra.encode.error.core.unknown_primitive_name_error" (func $hydra.encode.error.core.unknown_primitive_name_error) )
  (export "hydra.encode.error.core.unnecessary_identity_application_error" (func $hydra.encode.error.core.unnecessary_identity_application_error) )
  (export "hydra.encode.error.core.untyped_term_variable_error" (func $hydra.encode.error.core.untyped_term_variable_error) )
  (export "hydra.encode.error.core.void_in_non_bottom_position_error" (func $hydra.encode.error.core.void_in_non_bottom_position_error) )
  (func $hydra.encode.error.core.constant_condition_error (param $x i32) (result i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.error.core.ConstantConditionError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "value"
  local.get $x2
)
  (func $hydra.encode.error.core.duplicate_binding_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.DuplicateBindingError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.duplicate_field_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.DuplicateFieldError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.duplicate_record_type_field_names_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.DuplicateRecordTypeFieldNamesError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.duplicate_union_type_field_names_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.DuplicateUnionTypeFieldNamesError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.empty_case_statement_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.EmptyCaseStatementError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "typeName"
  local.get $x
  ;; project field: type_name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.empty_let_bindings_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.EmptyLetBindingsError"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
)
  (func $hydra.encode.error.core.empty_record_type_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.EmptyRecordTypeError"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
)
  (func $hydra.encode.error.core.empty_term_annotation_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.EmptyTermAnnotationError"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
)
  (func $hydra.encode.error.core.empty_type_annotation_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.EmptyTypeAnnotationError"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
)
  (func $hydra.encode.error.core.empty_type_name_in_term_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.EmptyTypeNameInTermError"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
)
  (func $hydra.encode.error.core.empty_union_type_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.EmptyUnionTypeError"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
)
  (func $hydra.encode.error.core.invalid_forall_parameter_name_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.InvalidForallParameterNameError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.invalid_lambda_parameter_name_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.InvalidLambdaParameterNameError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.invalid_let_binding_name_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.InvalidLetBindingNameError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.invalid_term_error (param $arg_0 i32) (result i32)
  (local $y i32)
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
  local.get $arg_0
  br_table $constant_condition $duplicate_binding $duplicate_field $empty_case_statement $empty_let_bindings $empty_term_annotation $empty_type_name_in_term $invalid_lambda_parameter_name $invalid_let_binding_name $invalid_type_lambda_parameter_name $nested_term_annotation $redundant_wrap_unwrap $self_application $term_variable_shadowing $type_variable_shadowing_in_type_lambda $undefined_term_variable $undefined_type_variable_in_binding_type $undefined_type_variable_in_lambda_domain $undefined_type_variable_in_type_application $unknown_primitive_name $unnecessary_identity_application $untyped_term_variable $untyped_term_variable
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTermError"
  i32.const 0 ;; string: "constantCondition"
  local.get $y
  call $hydra.encode.error.core.constant_condition_error
  br $end_invalid_term_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTermError"
  i32.const 0 ;; string: "duplicateBinding"
  local.get $y
  call $hydra.encode.error.core.duplicate_binding_error
  br $end_invalid_term_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTermError"
  i32.const 0 ;; string: "duplicateField"
  local.get $y
  call $hydra.encode.error.core.duplicate_field_error
  br $end_invalid_term_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTermError"
  i32.const 0 ;; string: "emptyCaseStatement"
  local.get $y
  call $hydra.encode.error.core.empty_case_statement_error
  br $end_invalid_term_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTermError"
  i32.const 0 ;; string: "emptyLetBindings"
  local.get $y
  call $hydra.encode.error.core.empty_let_bindings_error
  br $end_invalid_term_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTermError"
  i32.const 0 ;; string: "emptyTermAnnotation"
  local.get $y
  call $hydra.encode.error.core.empty_term_annotation_error
  br $end_invalid_term_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTermError"
  i32.const 0 ;; string: "emptyTypeNameInTerm"
  local.get $y
  call $hydra.encode.error.core.empty_type_name_in_term_error
  br $end_invalid_term_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTermError"
  i32.const 0 ;; string: "invalidLambdaParameterName"
  local.get $y
  call $hydra.encode.error.core.invalid_lambda_parameter_name_error
  br $end_invalid_term_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTermError"
  i32.const 0 ;; string: "invalidLetBindingName"
  local.get $y
  call $hydra.encode.error.core.invalid_let_binding_name_error
  br $end_invalid_term_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTermError"
  i32.const 0 ;; string: "invalidTypeLambdaParameterName"
  local.get $y
  call $hydra.encode.error.core.invalid_type_lambda_parameter_name_error
  br $end_invalid_term_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTermError"
  i32.const 0 ;; string: "nestedTermAnnotation"
  local.get $y
  call $hydra.encode.error.core.nested_term_annotation_error
  br $end_invalid_term_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTermError"
  i32.const 0 ;; string: "redundantWrapUnwrap"
  local.get $y
  call $hydra.encode.error.core.redundant_wrap_unwrap_error
  br $end_invalid_term_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTermError"
  i32.const 0 ;; string: "selfApplication"
  local.get $y
  call $hydra.encode.error.core.self_application_error
  br $end_invalid_term_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTermError"
  i32.const 0 ;; string: "termVariableShadowing"
  local.get $y
  call $hydra.encode.error.core.term_variable_shadowing_error
  br $end_invalid_term_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTermError"
  i32.const 0 ;; string: "typeVariableShadowingInTypeLambda"
  local.get $y
  call $hydra.encode.error.core.type_variable_shadowing_in_type_lambda_error
  br $end_invalid_term_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTermError"
  i32.const 0 ;; string: "undefinedTermVariable"
  local.get $y
  call $hydra.encode.error.core.undefined_term_variable_error
  br $end_invalid_term_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTermError"
  i32.const 0 ;; string: "undefinedTypeVariableInBindingType"
  local.get $y
  call $hydra.encode.error.core.undefined_type_variable_in_binding_type_error
  br $end_invalid_term_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTermError"
  i32.const 0 ;; string: "undefinedTypeVariableInLambdaDomain"
  local.get $y
  call $hydra.encode.error.core.undefined_type_variable_in_lambda_domain_error
  br $end_invalid_term_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTermError"
  i32.const 0 ;; string: "undefinedTypeVariableInTypeApplication"
  local.get $y
  call $hydra.encode.error.core.undefined_type_variable_in_type_application_error
  br $end_invalid_term_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTermError"
  i32.const 0 ;; string: "unknownPrimitiveName"
  local.get $y
  call $hydra.encode.error.core.unknown_primitive_name_error
  br $end_invalid_term_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTermError"
  i32.const 0 ;; string: "unnecessaryIdentityApplication"
  local.get $y
  call $hydra.encode.error.core.unnecessary_identity_application_error
  br $end_invalid_term_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTermError"
  i32.const 0 ;; string: "untypedTermVariable"
  local.get $y
  call $hydra.encode.error.core.untyped_term_variable_error
  br $end_invalid_term_error
)
)
  (func $hydra.encode.error.core.invalid_type_error (param $arg_0 i32) (result i32)
  (local $y i32)
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
  local.get $arg_0
  br_table $duplicate_record_type_field_names $duplicate_union_type_field_names $empty_record_type $empty_type_annotation $empty_union_type $invalid_forall_parameter_name $invalid_type_scheme_variable_name $nested_type_annotation $non_comparable_map_key_type $non_comparable_set_element_type $single_variant_union $type_variable_shadowing_in_forall $undefined_type_variable $void_in_non_bottom_position $void_in_non_bottom_position
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTypeError"
  i32.const 0 ;; string: "duplicateRecordTypeFieldNames"
  local.get $y
  call $hydra.encode.error.core.duplicate_record_type_field_names_error
  br $end_invalid_type_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTypeError"
  i32.const 0 ;; string: "duplicateUnionTypeFieldNames"
  local.get $y
  call $hydra.encode.error.core.duplicate_union_type_field_names_error
  br $end_invalid_type_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTypeError"
  i32.const 0 ;; string: "emptyRecordType"
  local.get $y
  call $hydra.encode.error.core.empty_record_type_error
  br $end_invalid_type_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTypeError"
  i32.const 0 ;; string: "emptyTypeAnnotation"
  local.get $y
  call $hydra.encode.error.core.empty_type_annotation_error
  br $end_invalid_type_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTypeError"
  i32.const 0 ;; string: "emptyUnionType"
  local.get $y
  call $hydra.encode.error.core.empty_union_type_error
  br $end_invalid_type_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTypeError"
  i32.const 0 ;; string: "invalidForallParameterName"
  local.get $y
  call $hydra.encode.error.core.invalid_forall_parameter_name_error
  br $end_invalid_type_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTypeError"
  i32.const 0 ;; string: "invalidTypeSchemeVariableName"
  local.get $y
  call $hydra.encode.error.core.invalid_type_scheme_variable_name_error
  br $end_invalid_type_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTypeError"
  i32.const 0 ;; string: "nestedTypeAnnotation"
  local.get $y
  call $hydra.encode.error.core.nested_type_annotation_error
  br $end_invalid_type_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTypeError"
  i32.const 0 ;; string: "nonComparableMapKeyType"
  local.get $y
  call $hydra.encode.error.core.non_comparable_map_key_type_error
  br $end_invalid_type_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTypeError"
  i32.const 0 ;; string: "nonComparableSetElementType"
  local.get $y
  call $hydra.encode.error.core.non_comparable_set_element_type_error
  br $end_invalid_type_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTypeError"
  i32.const 0 ;; string: "singleVariantUnion"
  local.get $y
  call $hydra.encode.error.core.single_variant_union_error
  br $end_invalid_type_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTypeError"
  i32.const 0 ;; string: "typeVariableShadowingInForall"
  local.get $y
  call $hydra.encode.error.core.type_variable_shadowing_in_forall_error
  br $end_invalid_type_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTypeError"
  i32.const 0 ;; string: "undefinedTypeVariable"
  local.get $y
  call $hydra.encode.error.core.undefined_type_variable_error
  br $end_invalid_type_error
)
  i32.const 0 ;; string: "hydra.error.core.InvalidTypeError"
  i32.const 0 ;; string: "voidInNonBottomPosition"
  local.get $y
  call $hydra.encode.error.core.void_in_non_bottom_position_error
  br $end_invalid_type_error
)
)
  (func $hydra.encode.error.core.invalid_type_lambda_parameter_name_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.InvalidTypeLambdaParameterNameError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.invalid_type_scheme_variable_name_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.InvalidTypeSchemeVariableNameError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.nested_term_annotation_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.NestedTermAnnotationError"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
)
  (func $hydra.encode.error.core.nested_type_annotation_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.NestedTypeAnnotationError"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
)
  (func $hydra.encode.error.core.non_comparable_map_key_type_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.NonComparableMapKeyTypeError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "keyType"
  local.get $x
  ;; project field: key_type
  call $hydra.encode.core.type
)
  (func $hydra.encode.error.core.non_comparable_set_element_type_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.NonComparableSetElementTypeError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "elementType"
  local.get $x
  ;; project field: element_type
  call $hydra.encode.core.type
)
  (func $hydra.encode.error.core.redundant_wrap_unwrap_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.RedundantWrapUnwrapError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "typeName"
  local.get $x
  ;; project field: type_name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.self_application_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.SelfApplicationError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.single_variant_union_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.SingleVariantUnionError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "fieldName"
  local.get $x
  ;; project field: field_name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.term_variable_shadowing_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.TermVariableShadowingError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.type_variable_shadowing_in_forall_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.TypeVariableShadowingInForallError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.type_variable_shadowing_in_type_lambda_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.TypeVariableShadowingInTypeLambdaError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.undefined_field_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.UndefinedFieldError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "fieldName"
  local.get $x
  ;; project field: field_name
  call $hydra.encode.core.name
  i32.const 0 ;; string: "typeName"
  local.get $x
  ;; project field: type_name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.undefined_term_variable_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.UndefinedTermVariableError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.undefined_type_variable_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.UndefinedTypeVariableError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.undefined_type_variable_in_binding_type_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.UndefinedTypeVariableInBindingTypeError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.undefined_type_variable_in_lambda_domain_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.UndefinedTypeVariableInLambdaDomainError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.undefined_type_variable_in_type_application_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.UndefinedTypeVariableInTypeApplicationError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.unexpected_term_variant_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.UnexpectedTermVariantError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "expectedVariant"
  local.get $x
  ;; project field: expected_variant
  call $hydra.encode.variants.term_variant
  i32.const 0 ;; string: "actualTerm"
  local.get $x
  ;; project field: actual_term
  call $hydra.encode.core.term
)
  (func $hydra.encode.error.core.unexpected_type_variant_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.UnexpectedTypeVariantError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "expectedVariant"
  local.get $x
  ;; project field: expected_variant
  call $hydra.encode.variants.type_variant
  i32.const 0 ;; string: "actualType"
  local.get $x
  ;; project field: actual_type
  call $hydra.encode.core.type
)
  (func $hydra.encode.error.core.unknown_primitive_name_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.UnknownPrimitiveNameError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.unnecessary_identity_application_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.UnnecessaryIdentityApplicationError"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
)
  (func $hydra.encode.error.core.untyped_term_variable_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.UntypedTermVariableError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
  i32.const 0 ;; string: "name"
  local.get $x
  ;; project field: name
  call $hydra.encode.core.name
)
  (func $hydra.encode.error.core.void_in_non_bottom_position_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.core.VoidInNonBottomPositionError"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "location"
  local.get $x
  ;; project field: location
  call $hydra.encode.paths.subterm_path
)
)
