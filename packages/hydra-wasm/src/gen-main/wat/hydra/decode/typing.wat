(module
  (import "hydra.decode.context" "hydra.decode.context.context" (func $hydra.decode.context.context (param i32) (result i32) ) )
  (import "hydra.decode.core" "hydra.decode.core.binding" (func $hydra.decode.core.binding (param i32) (result i32) ) )
  (import "hydra.decode.core" "hydra.decode.core.name" (func $hydra.decode.core.name (param i32) (result i32) ) )
  (import "hydra.decode.core" "hydra.decode.core.term" (func $hydra.decode.core.term (param i32) (result i32) ) )
  (import "hydra.decode.core" "hydra.decode.core.type" (func $hydra.decode.core.type (param i32) (result i32) ) )
  (import "hydra.decode.core" "hydra.decode.core.type_variable_metadata" (func $hydra.decode.core.type_variable_metadata (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_list" (func $hydra.extract.core.decode_list (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_map" (func $hydra.extract.core.decode_map (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_maybe" (func $hydra.extract.core.decode_maybe (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.require_field" (func $hydra.extract.core.require_field (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.to_field_map" (func $hydra.extract.core.to_field_map (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.strip_and_dereference_term_either" (func $hydra.lexical.strip_and_dereference_term_either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.decode.typing.function_structure" (func $hydra.decode.typing.function_structure) )
  (export "hydra.decode.typing.inference_result" (func $hydra.decode.typing.inference_result) )
  (export "hydra.decode.typing.term_subst" (func $hydra.decode.typing.term_subst) )
  (export "hydra.decode.typing.type_constraint" (func $hydra.decode.typing.type_constraint) )
  (export "hydra.decode.typing.type_subst" (func $hydra.decode.typing.type_subst) )
  (func $hydra.decode.typing.function_structure (param $env i32) (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_bindings i32)
  (local $field_body i32)
  (local $field_codomain i32)
  (local $field_domains i32)
  (local $field_environment i32)
  (local $field_map i32)
  (local $field_params i32)
  (local $field_type_params i32)
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
  i32.const 0 ;; string: "typeParams"
  call $hydra.decode.core.name
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "params"
  call $hydra.decode.core.name
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "bindings"
  call $hydra.decode.core.binding
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "body"
  call $hydra.decode.core.term
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "domains"
  call $hydra.decode.core.type
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "codomain"
  call $hydra.decode.core.type
  call $hydra.extract.core.decode_maybe
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "environment"
  local.get $env
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_type_params
  local.get $field_params
  local.get $field_bindings
  local.get $field_body
  local.get $field_domains
  local.get $field_codomain
  local.get $field_environment
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
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
  (func $hydra.decode.typing.inference_result (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_class_constraints i32)
  (local $field_context i32)
  (local $field_map i32)
  (local $field_subst i32)
  (local $field_term i32)
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
  i32.const 0 ;; string: "term"
  call $hydra.decode.core.term
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "type"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "subst"
  call $hydra.decode.typing.type_subst
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "classConstraints"
  call $hydra.decode.core.name
  call $hydra.decode.core.type_variable_metadata
  call $hydra.extract.core.decode_map
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "context"
  call $hydra.decode.context.context
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_term
  local.get $field_type
  local.get $field_subst
  local.get $field_class_constraints
  local.get $field_context
  call $hydra.lib.eithers.bind
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
  (func $hydra.decode.typing.term_subst (param $cx i32) (param $raw i32) (result i32)
  (local $b i32)
  (local $err i32)
  (local $stripped i32)
  (local $wrapped_term i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $wrap
  local.get $stripped
  br_table $wrap $wrap
)
  local.get $b
  call $hydra.decode.core.name
  call $hydra.decode.core.term
  local.get $cx
  local.get $wrapped_term
  ;; project field: body
  call $hydra.extract.core.decode_map
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.typing.type_constraint (param $cx i32) (param $raw i32) (result i32)
  (local $cx2 i32)
  (local $err i32)
  (local $field_comment i32)
  (local $field_left i32)
  (local $field_map i32)
  (local $field_right i32)
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
  i32.const 0 ;; string: "left"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "right"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "comment"
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
  local.get $field_left
  local.get $field_right
  local.get $field_comment
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
  (func $hydra.decode.typing.type_subst (param $cx i32) (param $raw i32) (result i32)
  (local $b i32)
  (local $err i32)
  (local $stripped i32)
  (local $wrapped_term i32)
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $wrap
  local.get $stripped
  br_table $wrap $wrap
)
  local.get $b
  call $hydra.decode.core.name
  call $hydra.decode.core.type
  local.get $cx
  local.get $wrapped_term
  ;; project field: body
  call $hydra.extract.core.decode_map
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
)
