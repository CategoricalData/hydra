(module
  (import "hydra.encode.core" "hydra.encode.core.binding" (func $hydra.encode.core.binding (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.name" (func $hydra.encode.core.name (param i32) (result i32) ) )
  (import "hydra.encode.core" "hydra.encode.core.type" (func $hydra.encode.core.type (param i32) (result i32) ) )
  (import "hydra.encode.typing" "hydra.encode.typing.type_subst" (func $hydra.encode.typing.type_subst (param i32) (result i32) ) )
  (import "hydra.encode.variants" "hydra.encode.variants.term_variant" (func $hydra.encode.variants.term_variant (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.map" (func $hydra.lib.sets.map (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.encode.error.checking.checking_error" (func $hydra.encode.error.checking.checking_error) )
  (export "hydra.encode.error.checking.incorrect_unification_error" (func $hydra.encode.error.checking.incorrect_unification_error) )
  (export "hydra.encode.error.checking.not_a_forall_type_error" (func $hydra.encode.error.checking.not_a_forall_type_error) )
  (export "hydra.encode.error.checking.not_a_function_type_error" (func $hydra.encode.error.checking.not_a_function_type_error) )
  (export "hydra.encode.error.checking.type_arity_mismatch_error" (func $hydra.encode.error.checking.type_arity_mismatch_error) )
  (export "hydra.encode.error.checking.type_mismatch_error" (func $hydra.encode.error.checking.type_mismatch_error) )
  (export "hydra.encode.error.checking.unbound_type_variables_error" (func $hydra.encode.error.checking.unbound_type_variables_error) )
  (export "hydra.encode.error.checking.unequal_types_error" (func $hydra.encode.error.checking.unequal_types_error) )
  (export "hydra.encode.error.checking.unsupported_term_variant_error" (func $hydra.encode.error.checking.unsupported_term_variant_error) )
  (export "hydra.encode.error.checking.untyped_lambda_error" (func $hydra.encode.error.checking.untyped_lambda_error) )
  (export "hydra.encode.error.checking.untyped_let_binding_error" (func $hydra.encode.error.checking.untyped_let_binding_error) )
  (func $hydra.encode.error.checking.checking_error (param $arg_0 i32) (result i32)
  (local $y i32)
  (block $end_checking_error (result i32)
  (block $untyped_let_binding
  (block $untyped_lambda
  (block $unsupported_term_variant
  (block $unequal_types
  (block $unbound_type_variables
  (block $type_mismatch
  (block $type_arity_mismatch
  (block $not_a_function_type
  (block $not_a_forall_type
  (block $incorrect_unification
  local.get $arg_0
  br_table $incorrect_unification $not_a_forall_type $not_a_function_type $type_arity_mismatch $type_mismatch $unbound_type_variables $unequal_types $unsupported_term_variant $untyped_lambda $untyped_let_binding $untyped_let_binding
)
  i32.const 0 ;; string: "hydra.error.checking.CheckingError"
  i32.const 0 ;; string: "incorrectUnification"
  local.get $y
  call $hydra.encode.error.checking.incorrect_unification_error
  br $end_checking_error
)
  i32.const 0 ;; string: "hydra.error.checking.CheckingError"
  i32.const 0 ;; string: "notAForallType"
  local.get $y
  call $hydra.encode.error.checking.not_a_forall_type_error
  br $end_checking_error
)
  i32.const 0 ;; string: "hydra.error.checking.CheckingError"
  i32.const 0 ;; string: "notAFunctionType"
  local.get $y
  call $hydra.encode.error.checking.not_a_function_type_error
  br $end_checking_error
)
  i32.const 0 ;; string: "hydra.error.checking.CheckingError"
  i32.const 0 ;; string: "typeArityMismatch"
  local.get $y
  call $hydra.encode.error.checking.type_arity_mismatch_error
  br $end_checking_error
)
  i32.const 0 ;; string: "hydra.error.checking.CheckingError"
  i32.const 0 ;; string: "typeMismatch"
  local.get $y
  call $hydra.encode.error.checking.type_mismatch_error
  br $end_checking_error
)
  i32.const 0 ;; string: "hydra.error.checking.CheckingError"
  i32.const 0 ;; string: "unboundTypeVariables"
  local.get $y
  call $hydra.encode.error.checking.unbound_type_variables_error
  br $end_checking_error
)
  i32.const 0 ;; string: "hydra.error.checking.CheckingError"
  i32.const 0 ;; string: "unequalTypes"
  local.get $y
  call $hydra.encode.error.checking.unequal_types_error
  br $end_checking_error
)
  i32.const 0 ;; string: "hydra.error.checking.CheckingError"
  i32.const 0 ;; string: "unsupportedTermVariant"
  local.get $y
  call $hydra.encode.error.checking.unsupported_term_variant_error
  br $end_checking_error
)
  i32.const 0 ;; string: "hydra.error.checking.CheckingError"
  i32.const 0 ;; string: "untypedLambda"
  local.get $y
  call $hydra.encode.error.checking.untyped_lambda_error
  br $end_checking_error
)
  i32.const 0 ;; string: "hydra.error.checking.CheckingError"
  i32.const 0 ;; string: "untypedLetBinding"
  local.get $y
  call $hydra.encode.error.checking.untyped_let_binding_error
  br $end_checking_error
)
)
  (func $hydra.encode.error.checking.incorrect_unification_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.checking.IncorrectUnificationError"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "substitution"
  local.get $x
  ;; project field: substitution
  call $hydra.encode.typing.type_subst
)
  (func $hydra.encode.error.checking.not_a_forall_type_error (param $x i32) (result i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.error.checking.NotAForallTypeError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "type"
  local.get $x
  ;; project field: type
  call $hydra.encode.core.type
  i32.const 0 ;; string: "typeArguments"
  call $hydra.encode.core.type
  local.get $xs
  call $hydra.lib.lists.map
)
  (func $hydra.encode.error.checking.not_a_function_type_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.checking.NotAFunctionTypeError"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "type"
  local.get $x
  ;; project field: type
  call $hydra.encode.core.type
)
  (func $hydra.encode.error.checking.type_arity_mismatch_error (param $x i32) (result i32)
  (local $x2 i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.error.checking.TypeArityMismatchError"
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "type"
  local.get $x
  ;; project field: type
  call $hydra.encode.core.type
  i32.const 0 ;; string: "expectedArity"
  local.get $x2
  i32.const 0 ;; string: "actualArity"
  local.get $x2
  i32.const 0 ;; string: "typeArguments"
  call $hydra.encode.core.type
  local.get $xs
  call $hydra.lib.lists.map
)
  (func $hydra.encode.error.checking.type_mismatch_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.checking.TypeMismatchError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "expectedType"
  local.get $x
  ;; project field: expected_type
  call $hydra.encode.core.type
  i32.const 0 ;; string: "actualType"
  local.get $x
  ;; project field: actual_type
  call $hydra.encode.core.type
)
  (func $hydra.encode.error.checking.unbound_type_variables_error (param $x i32) (result i32)
  (local $s i32)
  i32.const 0 ;; string: "hydra.error.checking.UnboundTypeVariablesError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "variables"
  call $hydra.encode.core.name
  local.get $s
  call $hydra.lib.sets.map
  i32.const 0 ;; string: "type"
  local.get $x
  ;; project field: type
  call $hydra.encode.core.type
)
  (func $hydra.encode.error.checking.unequal_types_error (param $x i32) (result i32)
  (local $x2 i32)
  (local $xs i32)
  i32.const 0 ;; string: "hydra.error.checking.UnequalTypesError"
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "types"
  call $hydra.encode.core.type
  local.get $xs
  call $hydra.lib.lists.map
  i32.const 0 ;; string: "description"
  local.get $x2
)
  (func $hydra.encode.error.checking.unsupported_term_variant_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.checking.UnsupportedTermVariantError"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "termVariant"
  local.get $x
  ;; project field: term_variant
  call $hydra.encode.variants.term_variant
)
  (func $hydra.encode.error.checking.untyped_lambda_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.checking.UntypedLambdaError"
  i32.const 0
  ;; list elements follow
)
  (func $hydra.encode.error.checking.untyped_let_binding_error (param $x i32) (result i32)
  i32.const 0 ;; string: "hydra.error.checking.UntypedLetBindingError"
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "binding"
  local.get $x
  ;; project field: binding
  call $hydra.encode.core.binding
)
)
