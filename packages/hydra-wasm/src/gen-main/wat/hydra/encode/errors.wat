(module
  (import "hydra.encode.core" "hydra.encode.core.type" (func $hydra.encode.core.type (param i32) (result i32) ) )
  (import "hydra.encode.error.checking" "hydra.encode.error.checking.checking_error" (func $hydra.encode.error.checking.checking_error (param i32) (result i32) ) )
  (import "hydra.encode.error.core" "hydra.encode.error.core.duplicate_binding_error" (func $hydra.encode.error.core.duplicate_binding_error (param i32) (result i32) ) )
  (import "hydra.encode.error.core" "hydra.encode.error.core.duplicate_field_error" (func $hydra.encode.error.core.duplicate_field_error (param i32) (result i32) ) )
  (import "hydra.encode.error.core" "hydra.encode.error.core.undefined_field_error" (func $hydra.encode.error.core.undefined_field_error (param i32) (result i32) ) )
  (import "hydra.encode.error.core" "hydra.encode.error.core.undefined_term_variable_error" (func $hydra.encode.error.core.undefined_term_variable_error (param i32) (result i32) ) )
  (import "hydra.encode.error.core" "hydra.encode.error.core.unexpected_term_variant_error" (func $hydra.encode.error.core.unexpected_term_variant_error (param i32) (result i32) ) )
  (import "hydra.encode.error.core" "hydra.encode.error.core.unexpected_type_variant_error" (func $hydra.encode.error.core.unexpected_type_variant_error (param i32) (result i32) ) )
  (import "hydra.encode.error.core" "hydra.encode.error.core.untyped_term_variable_error" (func $hydra.encode.error.core.untyped_term_variable_error (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.encode.errors.decoding_error" (func $hydra.encode.errors.decoding_error) )
  (export "hydra.encode.errors.error" (func $hydra.encode.errors.error) )
  (export "hydra.encode.errors.other_error" (func $hydra.encode.errors.other_error) )
  (export "hydra.encode.errors.unification_error" (func $hydra.encode.errors.unification_error) )
  (func $hydra.encode.errors.decoding_error (param $x i32) (result i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.errors.DecodingError"
  local.get $x2
)
  (func $hydra.encode.errors.error (param $arg_0 i32) (result i32)
  (local $y i32)
  (block $end_error (result i32)
  (block $unification
  (block $unexpected_type_variant
  (block $unexpected_term_variant
  (block $untyped_term_variable
  (block $undefined_term_variable
  (block $undefined_field
  (block $other
  (block $duplicate_field
  (block $duplicate_binding
  (block $decoding
  (block $checking
  local.get $arg_0
  br_table $checking $decoding $duplicate_binding $duplicate_field $other $undefined_field $undefined_term_variable $untyped_term_variable $unexpected_term_variant $unexpected_type_variant $unification $unification
)
  i32.const 0 ;; string: "hydra.errors.Error"
  i32.const 0 ;; string: "checking"
  local.get $y
  call $hydra.encode.error.checking.checking_error
  br $end_error
)
  i32.const 0 ;; string: "hydra.errors.Error"
  i32.const 0 ;; string: "decoding"
  local.get $y
  call $hydra.encode.errors.decoding_error
  br $end_error
)
  i32.const 0 ;; string: "hydra.errors.Error"
  i32.const 0 ;; string: "duplicateBinding"
  local.get $y
  call $hydra.encode.error.core.duplicate_binding_error
  br $end_error
)
  i32.const 0 ;; string: "hydra.errors.Error"
  i32.const 0 ;; string: "duplicateField"
  local.get $y
  call $hydra.encode.error.core.duplicate_field_error
  br $end_error
)
  i32.const 0 ;; string: "hydra.errors.Error"
  i32.const 0 ;; string: "other"
  local.get $y
  call $hydra.encode.errors.other_error
  br $end_error
)
  i32.const 0 ;; string: "hydra.errors.Error"
  i32.const 0 ;; string: "undefinedField"
  local.get $y
  call $hydra.encode.error.core.undefined_field_error
  br $end_error
)
  i32.const 0 ;; string: "hydra.errors.Error"
  i32.const 0 ;; string: "undefinedTermVariable"
  local.get $y
  call $hydra.encode.error.core.undefined_term_variable_error
  br $end_error
)
  i32.const 0 ;; string: "hydra.errors.Error"
  i32.const 0 ;; string: "untypedTermVariable"
  local.get $y
  call $hydra.encode.error.core.untyped_term_variable_error
  br $end_error
)
  i32.const 0 ;; string: "hydra.errors.Error"
  i32.const 0 ;; string: "unexpectedTermVariant"
  local.get $y
  call $hydra.encode.error.core.unexpected_term_variant_error
  br $end_error
)
  i32.const 0 ;; string: "hydra.errors.Error"
  i32.const 0 ;; string: "unexpectedTypeVariant"
  local.get $y
  call $hydra.encode.error.core.unexpected_type_variant_error
  br $end_error
)
  i32.const 0 ;; string: "hydra.errors.Error"
  i32.const 0 ;; string: "unification"
  local.get $y
  call $hydra.encode.errors.unification_error
  br $end_error
)
)
  (func $hydra.encode.errors.other_error (param $x i32) (result i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.errors.OtherError"
  local.get $x2
)
  (func $hydra.encode.errors.unification_error (param $x i32) (result i32)
  (local $x2 i32)
  i32.const 0 ;; string: "hydra.errors.UnificationError"
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "leftType"
  local.get $x
  ;; project field: left_type
  call $hydra.encode.core.type
  i32.const 0 ;; string: "rightType"
  local.get $x
  ;; project field: right_type
  call $hydra.encode.core.type
  i32.const 0 ;; string: "message"
  local.get $x2
)
)
