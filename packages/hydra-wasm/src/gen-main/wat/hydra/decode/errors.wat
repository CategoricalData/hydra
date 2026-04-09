(module
  (import "hydra.decode.core" "hydra.decode.core.type" (func $hydra.decode.core.type (param i32) (result i32) ) )
  (import "hydra.decode.error.checking" "hydra.decode.error.checking.checking_error" (func $hydra.decode.error.checking.checking_error (param i32) (result i32) ) )
  (import "hydra.decode.error.core" "hydra.decode.error.core.duplicate_binding_error" (func $hydra.decode.error.core.duplicate_binding_error (param i32) (result i32) ) )
  (import "hydra.decode.error.core" "hydra.decode.error.core.duplicate_field_error" (func $hydra.decode.error.core.duplicate_field_error (param i32) (result i32) ) )
  (import "hydra.decode.error.core" "hydra.decode.error.core.undefined_field_error" (func $hydra.decode.error.core.undefined_field_error (param i32) (result i32) ) )
  (import "hydra.decode.error.core" "hydra.decode.error.core.undefined_term_variable_error" (func $hydra.decode.error.core.undefined_term_variable_error (param i32) (result i32) ) )
  (import "hydra.decode.error.core" "hydra.decode.error.core.unexpected_term_variant_error" (func $hydra.decode.error.core.unexpected_term_variant_error (param i32) (result i32) ) )
  (import "hydra.decode.error.core" "hydra.decode.error.core.unexpected_type_variant_error" (func $hydra.decode.error.core.unexpected_type_variant_error (param i32) (result i32) ) )
  (import "hydra.decode.error.core" "hydra.decode.error.core.untyped_term_variable_error" (func $hydra.decode.error.core.untyped_term_variable_error (param i32) (result i32) ) )
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
  (export "hydra.decode.errors.decoding_error" (func $hydra.decode.errors.decoding_error) )
  (export "hydra.decode.errors.error" (func $hydra.decode.errors.error) )
  (export "hydra.decode.errors.other_error" (func $hydra.decode.errors.other_error) )
  (export "hydra.decode.errors.unification_error" (func $hydra.decode.errors.unification_error) )
  (func $hydra.decode.errors.decoding_error (param $cx i32) (param $raw i32) (result i32)
  (local $b i32)
  (local $cx2 i32)
  (local $err i32)
  (local $raw2 i32)
  (local $s i32)
  (local $stripped i32)
  (local $stripped2 i32)
  (local $v i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $wrap
  local.get $stripped
  br_table $wrap $wrap
)
  local.get $b
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
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.errors.error (param $cx i32) (param $raw i32) (result i32)
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
  i32.const 11
  ;; list elements follow
  i32.const 0 ;; string: "checking"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.checking.checking_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "decoding"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.errors.decoding_error
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
  i32.const 0 ;; string: "other"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.errors.other_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "undefinedField"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.undefined_field_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "undefinedTermVariable"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.undefined_term_variable_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "untypedTermVariable"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.untyped_term_variable_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "unexpectedTermVariant"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.unexpected_term_variant_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "unexpectedTypeVariant"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.error.core.unexpected_type_variant_error
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "unification"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.errors.unification_error
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
  (func $hydra.decode.errors.other_error (param $cx i32) (param $raw i32) (result i32)
  (local $b i32)
  (local $cx2 i32)
  (local $err i32)
  (local $raw2 i32)
  (local $s i32)
  (local $stripped i32)
  (local $stripped2 i32)
  (local $v i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $wrap
  local.get $stripped
  br_table $wrap $wrap
)
  local.get $b
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
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.errors.unification_error (param $cx i32) (param $raw i32) (result i32)
  (local $cx2 i32)
  (local $err i32)
  (local $field_left_type i32)
  (local $field_map i32)
  (local $field_message i32)
  (local $field_right_type i32)
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
  i32.const 0 ;; string: "leftType"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "rightType"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "message"
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
  local.get $field_left_type
  local.get $field_right_type
  local.get $field_message
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
)
