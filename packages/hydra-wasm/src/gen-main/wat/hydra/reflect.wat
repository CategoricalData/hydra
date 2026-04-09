(module
  (import "hydra.lib.lists" "hydra.lib.lists.concat" (func $hydra.lib.lists.concat (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.reflect.elimination_variant" (func $hydra.reflect.elimination_variant) )
  (export "hydra.reflect.elimination_variants" (func $hydra.reflect.elimination_variants) )
  (export "hydra.reflect.float_type_precision" (func $hydra.reflect.float_type_precision) )
  (export "hydra.reflect.float_types" (func $hydra.reflect.float_types) )
  (export "hydra.reflect.float_value_type" (func $hydra.reflect.float_value_type) )
  (export "hydra.reflect.function_variant" (func $hydra.reflect.function_variant) )
  (export "hydra.reflect.function_variants" (func $hydra.reflect.function_variants) )
  (export "hydra.reflect.integer_type_is_signed" (func $hydra.reflect.integer_type_is_signed) )
  (export "hydra.reflect.integer_type_precision" (func $hydra.reflect.integer_type_precision) )
  (export "hydra.reflect.integer_types" (func $hydra.reflect.integer_types) )
  (export "hydra.reflect.integer_value_type" (func $hydra.reflect.integer_value_type) )
  (export "hydra.reflect.literal_type" (func $hydra.reflect.literal_type) )
  (export "hydra.reflect.literal_type_variant" (func $hydra.reflect.literal_type_variant) )
  (export "hydra.reflect.literal_types" (func $hydra.reflect.literal_types) )
  (export "hydra.reflect.literal_variant" (func $hydra.reflect.literal_variant) )
  (export "hydra.reflect.literal_variants" (func $hydra.reflect.literal_variants) )
  (export "hydra.reflect.term_variant" (func $hydra.reflect.term_variant) )
  (export "hydra.reflect.term_variants" (func $hydra.reflect.term_variants) )
  (export "hydra.reflect.type_variant" (func $hydra.reflect.type_variant) )
  (export "hydra.reflect.type_variants" (func $hydra.reflect.type_variants) )
  (func $hydra.reflect.elimination_variant (param $arg_0 i32) (result i32)
  (block $end_elimination (result i32)
  (block $wrap
  (block $union
  (block $record
  local.get $arg_0
  br_table $record $union $wrap $wrap
)
  i32.const 0
  br $end_elimination
)
  i32.const 0
  br $end_elimination
)
  i32.const 0
  br $end_elimination
)
)
  (func $hydra.reflect.elimination_variants (result i32)
  i32.const 3
  ;; list elements follow
  i32.const 0
  i32.const 0
  i32.const 0
)
  (func $hydra.reflect.float_type_precision (param $arg_0 i32) (result i32)
  (block $end_float_type (result i32)
  (block $float64
  (block $float32
  (block $bigfloat
  local.get $arg_0
  br_table $bigfloat $float32 $float64 $float64
)
  i32.const 0
  br $end_float_type
)
  i32.const 32
  br $end_float_type
)
  i32.const 64
  br $end_float_type
)
)
  (func $hydra.reflect.float_types (result i32)
  i32.const 3
  ;; list elements follow
  i32.const 0
  i32.const 0
  i32.const 0
)
  (func $hydra.reflect.float_value_type (param $arg_0 i32) (result i32)
  (block $end_float_value (result i32)
  (block $float64
  (block $float32
  (block $bigfloat
  local.get $arg_0
  br_table $bigfloat $float32 $float64 $float64
)
  i32.const 0
  br $end_float_value
)
  i32.const 0
  br $end_float_value
)
  i32.const 0
  br $end_float_value
)
)
  (func $hydra.reflect.function_variant (param $arg_0 i32) (result i32)
  (block $end_function (result i32)
  (block $lambda
  (block $elimination
  local.get $arg_0
  br_table $elimination $lambda $lambda
)
  i32.const 0
  br $end_function
)
  i32.const 0
  br $end_function
)
)
  (func $hydra.reflect.function_variants (result i32)
  i32.const 2
  ;; list elements follow
  i32.const 0
  i32.const 0
)
  (func $hydra.reflect.integer_type_is_signed (param $arg_0 i32) (result i32)
  (block $end_integer_type (result i32)
  (block $uint64
  (block $uint32
  (block $uint16
  (block $uint8
  (block $int64
  (block $int32
  (block $int16
  (block $int8
  (block $bigint
  local.get $arg_0
  br_table $bigint $int8 $int16 $int32 $int64 $uint8 $uint16 $uint32 $uint64 $uint64
)
  i32.const 1
  br $end_integer_type
)
  i32.const 1
  br $end_integer_type
)
  i32.const 1
  br $end_integer_type
)
  i32.const 1
  br $end_integer_type
)
  i32.const 1
  br $end_integer_type
)
  i32.const 0
  br $end_integer_type
)
  i32.const 0
  br $end_integer_type
)
  i32.const 0
  br $end_integer_type
)
  i32.const 0
  br $end_integer_type
)
)
  (func $hydra.reflect.integer_type_precision (param $arg_0 i32) (result i32)
  (block $end_integer_type (result i32)
  (block $uint64
  (block $uint32
  (block $uint16
  (block $uint8
  (block $int64
  (block $int32
  (block $int16
  (block $int8
  (block $bigint
  local.get $arg_0
  br_table $bigint $int8 $int16 $int32 $int64 $uint8 $uint16 $uint32 $uint64 $uint64
)
  i32.const 0
  br $end_integer_type
)
  i32.const 8
  br $end_integer_type
)
  i32.const 16
  br $end_integer_type
)
  i32.const 32
  br $end_integer_type
)
  i32.const 64
  br $end_integer_type
)
  i32.const 8
  br $end_integer_type
)
  i32.const 16
  br $end_integer_type
)
  i32.const 32
  br $end_integer_type
)
  i32.const 64
  br $end_integer_type
)
)
  (func $hydra.reflect.integer_types (result i32)
  i32.const 9
  ;; list elements follow
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
)
  (func $hydra.reflect.integer_value_type (param $arg_0 i32) (result i32)
  (block $end_integer_value (result i32)
  (block $uint64
  (block $uint32
  (block $uint16
  (block $uint8
  (block $int64
  (block $int32
  (block $int16
  (block $int8
  (block $bigint
  local.get $arg_0
  br_table $bigint $int8 $int16 $int32 $int64 $uint8 $uint16 $uint32 $uint64 $uint64
)
  i32.const 0
  br $end_integer_value
)
  i32.const 0
  br $end_integer_value
)
  i32.const 0
  br $end_integer_value
)
  i32.const 0
  br $end_integer_value
)
  i32.const 0
  br $end_integer_value
)
  i32.const 0
  br $end_integer_value
)
  i32.const 0
  br $end_integer_value
)
  i32.const 0
  br $end_integer_value
)
  i32.const 0
  br $end_integer_value
)
)
  (func $hydra.reflect.literal_type (param $arg_0 i32) (result i32)
  (local $injected_ i32)
  (block $end_literal (result i32)
  (block $string
  (block $integer
  (block $float
  (block $boolean
  (block $binary
  local.get $arg_0
  br_table $binary $boolean $float $integer $string $string
)
  i32.const 0
  br $end_literal
)
  i32.const 0
  br $end_literal
)
  local.get $injected_
  br $end_literal
)
  local.get $injected_
  br $end_literal
)
  i32.const 0
  br $end_literal
)
)
  (func $hydra.reflect.literal_type_variant (param $arg_0 i32) (result i32)
  (block $end_literal_type (result i32)
  (block $string
  (block $integer
  (block $float
  (block $boolean
  (block $binary
  local.get $arg_0
  br_table $binary $boolean $float $integer $string $string
)
  i32.const 0
  br $end_literal_type
)
  i32.const 0
  br $end_literal_type
)
  i32.const 0
  br $end_literal_type
)
  i32.const 0
  br $end_literal_type
)
  i32.const 0
  br $end_literal_type
)
)
  (func $hydra.reflect.literal_types (result i32)
  (local $x i32)
  i32.const 4
  ;; list elements follow
  i32.const 2
  ;; list elements follow
  i32.const 0
  i32.const 0
  local.get $x
  call $hydra.reflect.float_types
  call $hydra.lib.lists.map
  local.get $x
  call $hydra.reflect.integer_types
  call $hydra.lib.lists.map
  i32.const 1
  ;; list elements follow
  i32.const 0
  call $hydra.lib.lists.concat
)
  (func $hydra.reflect.literal_variant (param $arg_ i32) (result i32)
  local.get $arg_
  call $hydra.reflect.literal_type
  call $hydra.reflect.literal_type_variant
)
  (func $hydra.reflect.literal_variants (result i32)
  i32.const 5
  ;; list elements follow
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
)
  (func $hydra.reflect.term_variant (param $arg_0 i32) (result i32)
  (block $end_term (result i32)
  (block $wrap
  (block $variable
  (block $unit
  (block $union
  (block $type_lambda
  (block $type_application
  (block $set
  (block $record
  (block $pair
  (block $maybe
  (block $map
  (block $literal
  (block $list
  (block $let
  (block $function
  (block $either
  (block $application
  (block $annotated
  local.get $arg_0
  br_table $annotated $application $either $function $let $list $literal $map $maybe $pair $record $set $type_application $type_lambda $union $unit $variable $wrap $wrap
)
  i32.const 0
  br $end_term
)
  i32.const 0
  br $end_term
)
  i32.const 0
  br $end_term
)
  i32.const 0
  br $end_term
)
  i32.const 0
  br $end_term
)
  i32.const 0
  br $end_term
)
  i32.const 0
  br $end_term
)
  i32.const 0
  br $end_term
)
  i32.const 0
  br $end_term
)
  i32.const 0
  br $end_term
)
  i32.const 0
  br $end_term
)
  i32.const 0
  br $end_term
)
  i32.const 0
  br $end_term
)
  i32.const 0
  br $end_term
)
  i32.const 0
  br $end_term
)
  i32.const 0
  br $end_term
)
  i32.const 0
  br $end_term
)
  i32.const 0
  br $end_term
)
)
  (func $hydra.reflect.term_variants (result i32)
  i32.const 17
  ;; list elements follow
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
)
  (func $hydra.reflect.type_variant (param $arg_0 i32) (result i32)
  (block $end_type (result i32)
  (block $wrap
  (block $void
  (block $variable
  (block $unit
  (block $union
  (block $set
  (block $record
  (block $pair
  (block $maybe
  (block $map
  (block $literal
  (block $list
  (block $forall
  (block $function
  (block $either
  (block $application
  (block $annotated
  local.get $arg_0
  br_table $annotated $application $either $function $forall $list $literal $map $maybe $pair $record $set $union $unit $variable $void $wrap $wrap
)
  i32.const 0
  br $end_type
)
  i32.const 0
  br $end_type
)
  i32.const 0
  br $end_type
)
  i32.const 0
  br $end_type
)
  i32.const 0
  br $end_type
)
  i32.const 0
  br $end_type
)
  i32.const 0
  br $end_type
)
  i32.const 0
  br $end_type
)
  i32.const 0
  br $end_type
)
  i32.const 0
  br $end_type
)
  i32.const 0
  br $end_type
)
  i32.const 0
  br $end_type
)
  i32.const 0
  br $end_type
)
  i32.const 0
  br $end_type
)
  i32.const 0
  br $end_type
)
  i32.const 0
  br $end_type
)
  i32.const 0
  br $end_type
)
)
  (func $hydra.reflect.type_variants (result i32)
  i32.const 17
  ;; list elements follow
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
  i32.const 0
)
)
