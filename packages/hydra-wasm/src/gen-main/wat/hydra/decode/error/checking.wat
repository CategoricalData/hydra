(module
  (import "hydra.decode.core" "hydra.decode.core.binding" (func $hydra.decode.core.binding (param i32) (result i32) ) )
  (import "hydra.decode.core" "hydra.decode.core.name" (func $hydra.decode.core.name (param i32) (result i32) ) )
  (import "hydra.decode.core" "hydra.decode.core.type" (func $hydra.decode.core.type (param i32) (result i32) ) )
  (import "hydra.decode.typing" "hydra.decode.typing.type_subst" (func $hydra.decode.typing.type_subst (param i32) (result i32) ) )
  (import "hydra.decode.variants" "hydra.decode.variants.term_variant" (func $hydra.decode.variants.term_variant (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_list" (func $hydra.extract.core.decode_list (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_set" (func $hydra.extract.core.decode_set (param i32) (result i32) ) )
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
  (export "hydra.decode.error.checking.checking_error" (func $hydra.decode.error.checking.checking_error) )
  (export "hydra.decode.error.checking.incorrect_unification_error" (func $hydra.decode.error.checking.incorrect_unification_error) )
  (export "hydra.decode.error.checking.not_a_forall_type_error" (func $hydra.decode.error.checking.not_a_forall_type_error) )
  (export "hydra.decode.error.checking.not_a_function_type_error" (func $hydra.decode.error.checking.not_a_function_type_error) )
  (export "hydra.decode.error.checking.type_arity_mismatch_error" (func $hydra.decode.error.checking.type_arity_mismatch_error) )
  (export "hydra.decode.error.checking.type_mismatch_error" (func $hydra.decode.error.checking.type_mismatch_error) )
  (export "hydra.decode.error.checking.unbound_type_variables_error" (func $hydra.decode.error.checking.unbound_type_variables_error) )
  (export "hydra.decode.error.checking.unequal_types_error" (func $hydra.decode.error.checking.unequal_types_error) )
  (export "hydra.decode.error.checking.unsupported_term_variant_error" (func $hydra.decode.error.checking.unsupported_term_variant_error) )
  (export "hydra.decode.error.checking.untyped_lambda_error" (func $hydra.decode.error.checking.untyped_lambda_error) )
  (export "hydra.decode.error.checking.untyped_let_binding_error" (func $hydra.decode.error.checking.untyped_let_binding_error) )
  (func $hydra.decode.error.checking.checking_error (param $cx i32) (param $raw i32) (result i32)
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
  i32.const 10
  ;; list elements follow
  i32.const 0 ;; string: "incorrectUnification"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.checking.incorrect_unification_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "notAForallType"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.checking.not_a_forall_type_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "notAFunctionType"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.checking.not_a_function_type_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "typeArityMismatch"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.checking.type_arity_mismatch_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "typeMismatch"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.checking.type_mismatch_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "unboundTypeVariables"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.checking.unbound_type_variables_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "unequalTypes"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.checking.unequal_types_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "unsupportedTermVariant"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.checking.unsupported_term_variant_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "untypedLambda"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.checking.untyped_lambda_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "untypedLetBinding"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.checking.untyped_let_binding_error
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
  (func $hydra.decode.error.checking.incorrect_unification_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_map i32)
  (local $field_substitution i32)
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
  i32.const 0 ;; string: "substitution"
  call $hydra.decode.typing.type_subst
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_substitution
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.checking.not_a_forall_type_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_map i32)
  (local $field_type i32)
  (local $field_type_arguments i32)
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
  i32.const 0 ;; string: "type"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "typeArguments"
  call $hydra.decode.core.type
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_type
  local.get $field_type_arguments
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.checking.not_a_function_type_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_map i32)
  (local $field_type i32)
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
  i32.const 0 ;; string: "type"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_type
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.checking.type_arity_mismatch_error (param $cx i32) (param $raw i32) (result i32)
  (local $cx2 i32)
  (local $err i32)
  (local $field_actual_arity i32)
  (local $field_expected_arity i32)
  (local $field_map i32)
  (local $field_type i32)
  (local $field_type_arguments i32)
  (local $i i32)
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
  i32.const 0 ;; string: "type"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "expectedArity"
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $integer
  local.get $v
  br_table $integer $integer
)
  (block $end_integer_value (result i32)
  (block $int32
  local.get $v
  br_table $int32 $int32
)
  i32.const 1
  local.get $i
  br $end_integer_value
)
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
  i32.const 0 ;; string: "actualArity"
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $integer
  local.get $v
  br_table $integer $integer
)
  (block $end_integer_value (result i32)
  (block $int32
  local.get $v
  br_table $int32 $int32
)
  i32.const 1
  local.get $i
  br $end_integer_value
)
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
  i32.const 0 ;; string: "typeArguments"
  call $hydra.decode.core.type
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_type
  local.get $field_expected_arity
  local.get $field_actual_arity
  local.get $field_type_arguments
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.checking.type_mismatch_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_actual_type i32)
  (local $field_expected_type i32)
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
  i32.const 0 ;; string: "expectedType"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "actualType"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_expected_type
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
  (func $hydra.decode.error.checking.unbound_type_variables_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_map i32)
  (local $field_type i32)
  (local $field_variables i32)
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
  i32.const 0 ;; string: "variables"
  call $hydra.decode.core.name
  call $hydra.extract.core.decode_set
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "type"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_variables
  local.get $field_type
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.checking.unequal_types_error (param $cx i32) (param $raw i32) (result i32)
  (local $cx2 i32)
  (local $err i32)
  (local $field_description i32)
  (local $field_map i32)
  (local $field_types i32)
  (local $raw2 i32)
  (local $record i32)
  (local $s i32)
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
  i32.const 0 ;; string: "types"
  call $hydra.decode.core.type
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "description"
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $string
  local.get $v
  br_table $string $string
)
  i32.const 1
  local.get $s
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
  local.get $field_types
  local.get $field_description
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.checking.unsupported_term_variant_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_map i32)
  (local $field_term_variant i32)
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
  i32.const 0 ;; string: "termVariant"
  call $hydra.decode.variants.term_variant
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_term_variant
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.checking.untyped_lambda_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
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
  i32.const 1
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.error.checking.untyped_let_binding_error (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_binding i32)
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
  i32.const 0 ;; string: "binding"
  call $hydra.decode.core.binding
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_binding
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
)
