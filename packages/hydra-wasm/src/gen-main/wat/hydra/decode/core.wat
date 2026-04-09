(module
  (import "hydra.extract.core" "hydra.extract.core.decode_either" (func $hydra.extract.core.decode_either (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_list" (func $hydra.extract.core.decode_list (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_map" (func $hydra.extract.core.decode_map (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_maybe" (func $hydra.extract.core.decode_maybe (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_pair" (func $hydra.extract.core.decode_pair (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_set" (func $hydra.extract.core.decode_set (param i32) (result i32) ) )
  (import "hydra.extract.core" "hydra.extract.core.decode_unit" (func $hydra.extract.core.decode_unit (param i32) (result i32) ) )
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
  (export "hydra.decode.core.annotated_term" (func $hydra.decode.core.annotated_term) )
  (export "hydra.decode.core.annotated_type" (func $hydra.decode.core.annotated_type) )
  (export "hydra.decode.core.application" (func $hydra.decode.core.application) )
  (export "hydra.decode.core.application_type" (func $hydra.decode.core.application_type) )
  (export "hydra.decode.core.binding" (func $hydra.decode.core.binding) )
  (export "hydra.decode.core.case_statement" (func $hydra.decode.core.case_statement) )
  (export "hydra.decode.core.either_type" (func $hydra.decode.core.either_type) )
  (export "hydra.decode.core.elimination" (func $hydra.decode.core.elimination) )
  (export "hydra.decode.core.field" (func $hydra.decode.core.field) )
  (export "hydra.decode.core.field_type" (func $hydra.decode.core.field_type) )
  (export "hydra.decode.core.float_type" (func $hydra.decode.core.float_type) )
  (export "hydra.decode.core.float_value" (func $hydra.decode.core.float_value) )
  (export "hydra.decode.core.forall_type" (func $hydra.decode.core.forall_type) )
  (export "hydra.decode.core.function" (func $hydra.decode.core.function) )
  (export "hydra.decode.core.function_type" (func $hydra.decode.core.function_type) )
  (export "hydra.decode.core.injection" (func $hydra.decode.core.injection) )
  (export "hydra.decode.core.integer_type" (func $hydra.decode.core.integer_type) )
  (export "hydra.decode.core.integer_value" (func $hydra.decode.core.integer_value) )
  (export "hydra.decode.core.lambda" (func $hydra.decode.core.lambda) )
  (export "hydra.decode.core.let" (func $hydra.decode.core.let) )
  (export "hydra.decode.core.literal" (func $hydra.decode.core.literal) )
  (export "hydra.decode.core.literal_type" (func $hydra.decode.core.literal_type) )
  (export "hydra.decode.core.map_type" (func $hydra.decode.core.map_type) )
  (export "hydra.decode.core.name" (func $hydra.decode.core.name) )
  (export "hydra.decode.core.pair_type" (func $hydra.decode.core.pair_type) )
  (export "hydra.decode.core.projection" (func $hydra.decode.core.projection) )
  (export "hydra.decode.core.record" (func $hydra.decode.core.record) )
  (export "hydra.decode.core.term" (func $hydra.decode.core.term) )
  (export "hydra.decode.core.type" (func $hydra.decode.core.type) )
  (export "hydra.decode.core.type_application_term" (func $hydra.decode.core.type_application_term) )
  (export "hydra.decode.core.type_lambda" (func $hydra.decode.core.type_lambda) )
  (export "hydra.decode.core.type_scheme" (func $hydra.decode.core.type_scheme) )
  (export "hydra.decode.core.type_variable_metadata" (func $hydra.decode.core.type_variable_metadata) )
  (export "hydra.decode.core.wrapped_term" (func $hydra.decode.core.wrapped_term) )
  (func $hydra.decode.core.annotated_term (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_annotation i32)
  (local $field_body i32)
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
  i32.const 0 ;; string: "body"
  call $hydra.decode.core.term
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "annotation"
  call $hydra.decode.core.name
  call $hydra.decode.core.term
  call $hydra.extract.core.decode_map
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_body
  local.get $field_annotation
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.core.annotated_type (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_annotation i32)
  (local $field_body i32)
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
  i32.const 0 ;; string: "body"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "annotation"
  call $hydra.decode.core.name
  call $hydra.decode.core.term
  call $hydra.extract.core.decode_map
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_body
  local.get $field_annotation
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.core.application (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_argument i32)
  (local $field_function i32)
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
  i32.const 0 ;; string: "function"
  call $hydra.decode.core.term
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "argument"
  call $hydra.decode.core.term
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_function
  local.get $field_argument
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.core.application_type (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_argument i32)
  (local $field_function i32)
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
  i32.const 0 ;; string: "function"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "argument"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_function
  local.get $field_argument
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.core.binding (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_map i32)
  (local $field_name i32)
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
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "term"
  call $hydra.decode.core.term
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "type"
  call $hydra.decode.core.type_scheme
  call $hydra.extract.core.decode_maybe
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_name
  local.get $field_term
  local.get $field_type
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
  (func $hydra.decode.core.case_statement (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_cases i32)
  (local $field_default i32)
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
  i32.const 0 ;; string: "typeName"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "default"
  call $hydra.decode.core.term
  call $hydra.extract.core.decode_maybe
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "cases"
  call $hydra.decode.core.field
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_type_name
  local.get $field_default
  local.get $field_cases
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
  (func $hydra.decode.core.either_type (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_left i32)
  (local $field_map i32)
  (local $field_right i32)
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
  i32.const 1
  local.get $field_left
  local.get $field_right
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.core.elimination (param $cx i32) (param $raw i32) (result i32)
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
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "record"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.projection
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "union"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.case_statement
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "wrap"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.name
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
  (func $hydra.decode.core.field (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_map i32)
  (local $field_name i32)
  (local $field_term i32)
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
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "term"
  call $hydra.decode.core.term
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_name
  local.get $field_term
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.core.field_type (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_map i32)
  (local $field_name i32)
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
  i32.const 0 ;; string: "name"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "type"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_name
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
  (func $hydra.decode.core.float_type (param $cx i32) (param $raw i32) (result i32)
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
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "bigfloat"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "float32"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "float64"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
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
  (func $hydra.decode.core.float_value (param $cx i32) (param $raw i32) (result i32)
  (local $cx2 i32)
  (local $err i32)
  (local $f i32)
  (local $field i32)
  (local $fname i32)
  (local $fterm i32)
  (local $inj i32)
  (local $raw2 i32)
  (local $stripped i32)
  (local $stripped2 i32)
  (local $t i32)
  (local $v i32)
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
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "bigfloat"
  local.get $t
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $float
  local.get $v
  br_table $float $float
)
  (block $end_float_value (result i32)
  (block $bigfloat
  local.get $v
  br_table $bigfloat $bigfloat
)
  i32.const 1
  local.get $f
  br $end_float_value
)
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "float32"
  local.get $t
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $float
  local.get $v
  br_table $float $float
)
  (block $end_float_value (result i32)
  (block $float32
  local.get $v
  br_table $float32 $float32
)
  i32.const 1
  local.get $f
  br $end_float_value
)
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "float64"
  local.get $t
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $float
  local.get $v
  br_table $float $float
)
  (block $end_float_value (result i32)
  (block $float64
  local.get $v
  br_table $float64 $float64
)
  i32.const 1
  local.get $f
  br $end_float_value
)
  br $end_literal
)
  br $end_term
)
  local.get $cx2
  local.get $raw2
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
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
  (func $hydra.decode.core.forall_type (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_body i32)
  (local $field_map i32)
  (local $field_parameter i32)
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
  i32.const 0 ;; string: "parameter"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "body"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_parameter
  local.get $field_body
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.core.function (param $cx i32) (param $raw i32) (result i32)
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
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "elimination"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.elimination
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "lambda"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.lambda
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
  (func $hydra.decode.core.function_type (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_codomain i32)
  (local $field_domain i32)
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
  i32.const 0 ;; string: "domain"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "codomain"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_domain
  local.get $field_codomain
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.core.injection (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_field i32)
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
  i32.const 0 ;; string: "typeName"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "field"
  call $hydra.decode.core.field
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_type_name
  local.get $field_field
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.core.integer_type (param $cx i32) (param $raw i32) (result i32)
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
  i32.const 9
  ;; list elements follow
  i32.const 0 ;; string: "bigint"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "int8"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "int16"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "int32"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "int64"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "uint8"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "uint16"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "uint32"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "uint64"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
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
  (func $hydra.decode.core.integer_value (param $cx i32) (param $raw i32) (result i32)
  (local $cx2 i32)
  (local $err i32)
  (local $f i32)
  (local $field i32)
  (local $fname i32)
  (local $fterm i32)
  (local $i i32)
  (local $inj i32)
  (local $raw2 i32)
  (local $stripped i32)
  (local $stripped2 i32)
  (local $t i32)
  (local $v i32)
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
  i32.const 9
  ;; list elements follow
  i32.const 0 ;; string: "bigint"
  local.get $t
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
  (block $bigint
  local.get $v
  br_table $bigint $bigint
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
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "int8"
  local.get $t
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
  (block $int8
  local.get $v
  br_table $int8 $int8
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
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "int16"
  local.get $t
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
  (block $int16
  local.get $v
  br_table $int16 $int16
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
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "int32"
  local.get $t
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
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "int64"
  local.get $t
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
  (block $int64
  local.get $v
  br_table $int64 $int64
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
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "uint8"
  local.get $t
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
  (block $uint8
  local.get $v
  br_table $uint8 $uint8
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
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "uint16"
  local.get $t
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
  (block $uint16
  local.get $v
  br_table $uint16 $uint16
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
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "uint32"
  local.get $t
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
  (block $uint32
  local.get $v
  br_table $uint32 $uint32
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
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "uint64"
  local.get $t
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
  (block $uint64
  local.get $v
  br_table $uint64 $uint64
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
  (func $hydra.decode.core.lambda (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_body i32)
  (local $field_domain i32)
  (local $field_map i32)
  (local $field_parameter i32)
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
  i32.const 0 ;; string: "parameter"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "domain"
  call $hydra.decode.core.type
  call $hydra.extract.core.decode_maybe
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "body"
  call $hydra.decode.core.term
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_parameter
  local.get $field_domain
  local.get $field_body
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
  (func $hydra.decode.core.let (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_bindings i32)
  (local $field_body i32)
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
  i32.const 1
  local.get $field_bindings
  local.get $field_body
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.core.literal (param $cx i32) (param $raw i32) (result i32)
  (local $b i32)
  (local $cx2 i32)
  (local $err i32)
  (local $f i32)
  (local $field i32)
  (local $fname i32)
  (local $fterm i32)
  (local $inj i32)
  (local $input i32)
  (local $raw2 i32)
  (local $s i32)
  (local $stripped i32)
  (local $stripped2 i32)
  (local $t i32)
  (local $v i32)
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
  i32.const 5
  ;; list elements follow
  i32.const 0 ;; string: "binary"
  local.get $t
  i32.const 0
  local.get $err
  (block $end_term (result i32)
  (block $literal
  local.get $stripped2
  br_table $literal $literal
)
  (block $end_literal (result i32)
  (block $binary
  local.get $v
  br_table $binary $binary
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
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "boolean"
  local.get $t
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
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "float"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.float_value
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "integer"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.integer_value
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "string"
  local.get $t
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
  (func $hydra.decode.core.literal_type (param $cx i32) (param $raw i32) (result i32)
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
  i32.const 5
  ;; list elements follow
  i32.const 0 ;; string: "binary"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "boolean"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "float"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.float_type
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "integer"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.integer_type
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "string"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
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
  (func $hydra.decode.core.map_type (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_keys i32)
  (local $field_map i32)
  (local $field_values i32)
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
  i32.const 0 ;; string: "keys"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "values"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_keys
  local.get $field_values
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.core.name (param $cx i32) (param $raw i32) (result i32)
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
  (func $hydra.decode.core.pair_type (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_first i32)
  (local $field_map i32)
  (local $field_second i32)
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
  i32.const 0 ;; string: "first"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "second"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_first
  local.get $field_second
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.core.projection (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_field i32)
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
  i32.const 0 ;; string: "typeName"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "field"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_type_name
  local.get $field_field
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.core.record (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_fields i32)
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
  i32.const 0 ;; string: "typeName"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "fields"
  call $hydra.decode.core.field
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_type_name
  local.get $field_fields
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.core.term (param $cx i32) (param $raw i32) (result i32)
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
  i32.const 18
  ;; list elements follow
  i32.const 0 ;; string: "annotated"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.annotated_term
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "application"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.application
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "either"
  local.get $t
  call $hydra.decode.core.term
  call $hydra.decode.core.term
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_either
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "function"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.function
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "let"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.let
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "list"
  local.get $t
  call $hydra.decode.core.term
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_list
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "literal"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.literal
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "map"
  local.get $t
  call $hydra.decode.core.term
  call $hydra.decode.core.term
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_map
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "maybe"
  local.get $t
  call $hydra.decode.core.term
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_maybe
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "pair"
  local.get $t
  call $hydra.decode.core.term
  call $hydra.decode.core.term
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_pair
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "record"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.record
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "set"
  local.get $t
  call $hydra.decode.core.term
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_set
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "typeApplication"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.type_application_term
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "typeLambda"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.type_lambda
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "union"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.injection
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "unit"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "variable"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.name
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "wrap"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.wrapped_term
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
  (func $hydra.decode.core.type (param $cx i32) (param $raw i32) (result i32)
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
  i32.const 17
  ;; list elements follow
  i32.const 0 ;; string: "annotated"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.annotated_type
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "application"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.application_type
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "either"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.either_type
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "forall"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.forall_type
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "function"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.function_type
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "list"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.type
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "literal"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.literal_type
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "map"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.map_type
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "maybe"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.type
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "pair"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.pair_type
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "record"
  local.get $t
  call $hydra.decode.core.field_type
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_list
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "set"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.type
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "union"
  local.get $t
  call $hydra.decode.core.field_type
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_list
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "unit"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "variable"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.name
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "void"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.extract.core.decode_unit
  call $hydra.lib.eithers.map
  i32.const 0 ;; string: "wrap"
  local.get $t
  local.get $cx
  local.get $input
  call $hydra.decode.core.type
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
  (func $hydra.decode.core.type_application_term (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_body i32)
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
  i32.const 0 ;; string: "body"
  call $hydra.decode.core.term
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "type"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_body
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
  (func $hydra.decode.core.type_lambda (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_body i32)
  (local $field_map i32)
  (local $field_parameter i32)
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
  i32.const 0 ;; string: "parameter"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "body"
  call $hydra.decode.core.term
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_parameter
  local.get $field_body
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.core.type_scheme (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_constraints i32)
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
  call $hydra.extract.core.decode_list
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "type"
  call $hydra.decode.core.type
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "constraints"
  call $hydra.decode.core.name
  call $hydra.decode.core.type_variable_metadata
  call $hydra.extract.core.decode_map
  call $hydra.extract.core.decode_maybe
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_variables
  local.get $field_type
  local.get $field_constraints
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
  (func $hydra.decode.core.type_variable_metadata (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_classes i32)
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
  i32.const 0 ;; string: "classes"
  call $hydra.decode.core.name
  call $hydra.extract.core.decode_set
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_classes
  call $hydra.lib.eithers.bind
  br $end_term
)
  local.get $cx
  local.get $raw
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.either
)
  (func $hydra.decode.core.wrapped_term (param $cx i32) (param $raw i32) (result i32)
  (local $err i32)
  (local $field_body i32)
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
  i32.const 0 ;; string: "typeName"
  call $hydra.decode.core.name
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 0 ;; string: "body"
  call $hydra.decode.core.term
  local.get $field_map
  local.get $cx
  call $hydra.extract.core.require_field
  i32.const 1
  local.get $field_type_name
  local.get $field_body
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
