(module
  (import "hydra.formatting" "hydra.formatting.show_list" (func $hydra.formatting.show_list (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.intercalate" (func $hydra.lib.strings.intercalate (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.binding" (func $hydra.show.core.binding (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.type" (func $hydra.show.core.type (param i32) (result i32) ) )
  (import "hydra.show.error.core" "hydra.show.error.core.duplicate_binding_error" (func $hydra.show.error.core.duplicate_binding_error (param i32) (result i32) ) )
  (import "hydra.show.error.core" "hydra.show.error.core.duplicate_field_error" (func $hydra.show.error.core.duplicate_field_error (param i32) (result i32) ) )
  (import "hydra.show.error.core" "hydra.show.error.core.undefined_field_error" (func $hydra.show.error.core.undefined_field_error (param i32) (result i32) ) )
  (import "hydra.show.error.core" "hydra.show.error.core.undefined_term_variable_error" (func $hydra.show.error.core.undefined_term_variable_error (param i32) (result i32) ) )
  (import "hydra.show.error.core" "hydra.show.error.core.unexpected_term_variant_error" (func $hydra.show.error.core.unexpected_term_variant_error (param i32) (result i32) ) )
  (import "hydra.show.error.core" "hydra.show.error.core.unexpected_type_variant_error" (func $hydra.show.error.core.unexpected_type_variant_error (param i32) (result i32) ) )
  (import "hydra.show.error.core" "hydra.show.error.core.untyped_term_variable_error" (func $hydra.show.error.core.untyped_term_variable_error (param i32) (result i32) ) )
  (import "hydra.show.typing" "hydra.show.typing.type_subst" (func $hydra.show.typing.type_subst (param i32) (result i32) ) )
  (import "hydra.show.variants" "hydra.show.variants.term_variant" (func $hydra.show.variants.term_variant (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.show.errors.checking_error" (func $hydra.show.errors.checking_error) )
  (export "hydra.show.errors.decoding_error" (func $hydra.show.errors.decoding_error) )
  (export "hydra.show.errors.error" (func $hydra.show.errors.error) )
  (export "hydra.show.errors.incorrect_unification_error" (func $hydra.show.errors.incorrect_unification_error) )
  (export "hydra.show.errors.not_a_forall_type_error" (func $hydra.show.errors.not_a_forall_type_error) )
  (export "hydra.show.errors.not_a_function_type_error" (func $hydra.show.errors.not_a_function_type_error) )
  (export "hydra.show.errors.other_error" (func $hydra.show.errors.other_error) )
  (export "hydra.show.errors.type_arity_mismatch_error" (func $hydra.show.errors.type_arity_mismatch_error) )
  (export "hydra.show.errors.type_mismatch_error" (func $hydra.show.errors.type_mismatch_error) )
  (export "hydra.show.errors.unbound_type_variables_error" (func $hydra.show.errors.unbound_type_variables_error) )
  (export "hydra.show.errors.unequal_types_error" (func $hydra.show.errors.unequal_types_error) )
  (export "hydra.show.errors.unification_error" (func $hydra.show.errors.unification_error) )
  (export "hydra.show.errors.unsupported_term_variant_error" (func $hydra.show.errors.unsupported_term_variant_error) )
  (export "hydra.show.errors.untyped_lambda_error" (func $hydra.show.errors.untyped_lambda_error) )
  (export "hydra.show.errors.untyped_let_binding_error" (func $hydra.show.errors.untyped_let_binding_error) )
  (func $hydra.show.errors.checking_error (param $ce i32) (result i32)
  (local $v i32)
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
  local.get $ce
  br_table $incorrect_unification $not_a_forall_type $not_a_function_type $type_arity_mismatch $type_mismatch $unbound_type_variables $unequal_types $unsupported_term_variant $untyped_lambda $untyped_let_binding $untyped_let_binding
)
  local.get $v
  call $hydra.show.errors.incorrect_unification_error
  br $end_checking_error
)
  local.get $v
  call $hydra.show.errors.not_a_forall_type_error
  br $end_checking_error
)
  local.get $v
  call $hydra.show.errors.not_a_function_type_error
  br $end_checking_error
)
  local.get $v
  call $hydra.show.errors.type_arity_mismatch_error
  br $end_checking_error
)
  local.get $v
  call $hydra.show.errors.type_mismatch_error
  br $end_checking_error
)
  local.get $v
  call $hydra.show.errors.unbound_type_variables_error
  br $end_checking_error
)
  local.get $v
  call $hydra.show.errors.unequal_types_error
  br $end_checking_error
)
  local.get $v
  call $hydra.show.errors.unsupported_term_variant_error
  br $end_checking_error
)
  local.get $v
  call $hydra.show.errors.untyped_lambda_error
  br $end_checking_error
)
  local.get $v
  call $hydra.show.errors.untyped_let_binding_error
  br $end_checking_error
)
)
  (func $hydra.show.errors.decoding_error (param $de i32) (result i32)
  i32.const 0 ;; string: "decoding error: "
  nop
  call $hydra.lib.strings.cat2
)
  (func $hydra.show.errors.error (param $e i32) (result i32)
  (local $v i32)
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
  local.get $e
  br_table $checking $decoding $duplicate_binding $duplicate_field $other $undefined_field $undefined_term_variable $untyped_term_variable $unexpected_term_variant $unexpected_type_variant $unification $unification
)
  local.get $v
  call $hydra.show.errors.checking_error
  br $end_error
)
  local.get $v
  call $hydra.show.errors.decoding_error
  br $end_error
)
  local.get $v
  call $hydra.show.error.core.duplicate_binding_error
  br $end_error
)
  local.get $v
  call $hydra.show.error.core.duplicate_field_error
  br $end_error
)
  local.get $v
  call $hydra.show.errors.other_error
  br $end_error
)
  local.get $v
  call $hydra.show.error.core.undefined_field_error
  br $end_error
)
  local.get $v
  call $hydra.show.error.core.undefined_term_variable_error
  br $end_error
)
  local.get $v
  call $hydra.show.error.core.untyped_term_variable_error
  br $end_error
)
  local.get $v
  call $hydra.show.error.core.unexpected_term_variant_error
  br $end_error
)
  local.get $v
  call $hydra.show.error.core.unexpected_type_variant_error
  br $end_error
)
  local.get $v
  call $hydra.show.errors.unification_error
  br $end_error
)
)
  (func $hydra.show.errors.incorrect_unification_error (param $e i32) (result i32)
  (local $subst i32)
  local.get $e
  ;; project field: substitution
  local.set $subst
  i32.const 0 ;; string: "incorrect unification: "
  local.get $subst
  call $hydra.show.typing.type_subst
  call $hydra.lib.strings.cat2
)
  (func $hydra.show.errors.not_a_forall_type_error (param $e i32) (result i32)
  (local $args i32)
  (local $typ i32)
  local.get $e
  ;; project field: type
  local.set $typ
  local.get $e
  ;; project field: type_arguments
  local.set $args
  i32.const 6
  ;; list elements follow
  i32.const 0 ;; string: "not a forall type: "
  local.get $typ
  call $hydra.show.core.type
  i32.const 0 ;; string: ". Trying to apply "
  local.get $args
  call $hydra.lib.lists.length
  call $hydra.lib.literals.show_int32
  i32.const 0 ;; string: " type argument(s): "
  call $hydra.show.core.type
  local.get $args
  call $hydra.formatting.show_list
  call $hydra.lib.strings.cat
)
  (func $hydra.show.errors.not_a_function_type_error (param $e i32) (result i32)
  (local $typ i32)
  local.get $e
  ;; project field: type
  local.set $typ
  i32.const 0 ;; string: "not a function type: "
  local.get $typ
  call $hydra.show.core.type
  call $hydra.lib.strings.cat2
)
  (func $hydra.show.errors.other_error (param $oe i32) (result i32)
  nop
)
  (func $hydra.show.errors.type_arity_mismatch_error (param $e i32) (result i32)
  (local $actual i32)
  (local $args i32)
  (local $expected i32)
  (local $typ i32)
  local.get $e
  ;; project field: type
  local.set $typ
  local.get $e
  ;; project field: expected_arity
  local.set $expected
  local.get $e
  ;; project field: actual_arity
  local.set $actual
  local.get $e
  ;; project field: type_arguments
  local.set $args
  i32.const 8
  ;; list elements follow
  i32.const 0 ;; string: "type "
  local.get $typ
  call $hydra.show.core.type
  i32.const 0 ;; string: " applied to the wrong number of type arguments (expected "
  local.get $expected
  call $hydra.lib.literals.show_int32
  i32.const 0 ;; string: ", got "
  local.get $actual
  call $hydra.lib.literals.show_int32
  i32.const 0 ;; string: "): "
  call $hydra.show.core.type
  local.get $args
  call $hydra.formatting.show_list
  call $hydra.lib.strings.cat
)
  (func $hydra.show.errors.type_mismatch_error (param $e i32) (result i32)
  (local $actual i32)
  (local $expected i32)
  local.get $e
  ;; project field: expected_type
  local.set $expected
  local.get $e
  ;; project field: actual_type
  local.set $actual
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "type mismatch: expected "
  local.get $expected
  call $hydra.show.core.type
  i32.const 0 ;; string: " but found "
  local.get $actual
  call $hydra.show.core.type
  call $hydra.lib.strings.cat
)
  (func $hydra.show.errors.unbound_type_variables_error (param $e i32) (result i32)
  (local $typ i32)
  (local $vars i32)
  local.get $e
  ;; project field: variables
  local.set $vars
  local.get $e
  ;; project field: type
  local.set $typ
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "unbound type variables: {"
  i32.const 0 ;; string: ", "
  nop
  local.get $vars
  call $hydra.lib.sets.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.strings.intercalate
  i32.const 0 ;; string: "} in type "
  local.get $typ
  call $hydra.show.core.type
  call $hydra.lib.strings.cat
)
  (func $hydra.show.errors.unequal_types_error (param $e i32) (result i32)
  (local $desc i32)
  (local $types i32)
  local.get $e
  ;; project field: types
  local.set $types
  local.get $e
  ;; project field: description
  local.set $desc
  i32.const 4
  ;; list elements follow
  i32.const 0 ;; string: "unequal types "
  call $hydra.show.core.type
  local.get $types
  call $hydra.formatting.show_list
  i32.const 0 ;; string: " in "
  local.get $desc
  call $hydra.lib.strings.cat
)
  (func $hydra.show.errors.unification_error (param $e i32) (result i32)
  (local $lt i32)
  (local $msg i32)
  (local $rt i32)
  local.get $e
  ;; project field: left_type
  local.set $lt
  local.get $e
  ;; project field: right_type
  local.set $rt
  local.get $e
  ;; project field: message
  local.set $msg
  i32.const 6
  ;; list elements follow
  i32.const 0 ;; string: "unification error: cannot unify "
  local.get $lt
  call $hydra.show.core.type
  i32.const 0 ;; string: " with "
  local.get $rt
  call $hydra.show.core.type
  i32.const 0 ;; string: ": "
  local.get $msg
  call $hydra.lib.strings.cat
)
  (func $hydra.show.errors.unsupported_term_variant_error (param $e i32) (result i32)
  i32.const 0 ;; string: "unsupported term variant: "
  local.get $e
  ;; project field: term_variant
  call $hydra.show.variants.term_variant
  call $hydra.lib.strings.cat2
)
  (func $hydra.show.errors.untyped_lambda_error (param $_ i32) (result i32)
  i32.const 0 ;; string: "untyped lambda"
)
  (func $hydra.show.errors.untyped_let_binding_error (param $e i32) (result i32)
  (local $b i32)
  local.get $e
  ;; project field: binding
  local.set $b
  i32.const 0 ;; string: "untyped let binding: "
  local.get $b
  call $hydra.show.core.binding
  call $hydra.lib.strings.cat2
)
)
