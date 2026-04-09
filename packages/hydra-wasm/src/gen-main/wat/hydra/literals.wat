(module
  (import "hydra.lib.literals" "hydra.lib.literals.bigfloat_to_float32" (func $hydra.lib.literals.bigfloat_to_float32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigfloat_to_float64" (func $hydra.lib.literals.bigfloat_to_float64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_int16" (func $hydra.lib.literals.bigint_to_int16 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_int32" (func $hydra.lib.literals.bigint_to_int32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_int64" (func $hydra.lib.literals.bigint_to_int64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_int8" (func $hydra.lib.literals.bigint_to_int8 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_uint16" (func $hydra.lib.literals.bigint_to_uint16 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_uint32" (func $hydra.lib.literals.bigint_to_uint32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_uint64" (func $hydra.lib.literals.bigint_to_uint64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_uint8" (func $hydra.lib.literals.bigint_to_uint8 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.float32_to_bigfloat" (func $hydra.lib.literals.float32_to_bigfloat (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.float64_to_bigfloat" (func $hydra.lib.literals.float64_to_bigfloat (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.int16_to_bigint" (func $hydra.lib.literals.int16_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.int32_to_bigint" (func $hydra.lib.literals.int32_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.int64_to_bigint" (func $hydra.lib.literals.int64_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.int8_to_bigint" (func $hydra.lib.literals.int8_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.uint16_to_bigint" (func $hydra.lib.literals.uint16_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.uint32_to_bigint" (func $hydra.lib.literals.uint32_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.uint64_to_bigint" (func $hydra.lib.literals.uint64_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.uint8_to_bigint" (func $hydra.lib.literals.uint8_to_bigint (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.literals.bigfloat_to_float_value" (func $hydra.literals.bigfloat_to_float_value) )
  (export "hydra.literals.bigint_to_integer_value" (func $hydra.literals.bigint_to_integer_value) )
  (export "hydra.literals.float_value_to_bigfloat" (func $hydra.literals.float_value_to_bigfloat) )
  (export "hydra.literals.integer_value_to_bigint" (func $hydra.literals.integer_value_to_bigint) )
  (func $hydra.literals.bigfloat_to_float_value (param $ft i32) (param $bf f64) (result i32)
  (block $end_float_type (result i32)
  (block $float64
  (block $float32
  (block $bigfloat
  local.get $ft
  br_table $bigfloat $float32 $float64 $float64
)
  local.get $bf
  br $end_float_type
)
  local.get $bf
  call $hydra.lib.literals.bigfloat_to_float32
  br $end_float_type
)
  local.get $bf
  call $hydra.lib.literals.bigfloat_to_float64
  br $end_float_type
)
)
  (func $hydra.literals.bigint_to_integer_value (param $it i32) (param $bi i32) (result i32)
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
  local.get $it
  br_table $bigint $int8 $int16 $int32 $int64 $uint8 $uint16 $uint32 $uint64 $uint64
)
  local.get $bi
  br $end_integer_type
)
  local.get $bi
  call $hydra.lib.literals.bigint_to_int8
  br $end_integer_type
)
  local.get $bi
  call $hydra.lib.literals.bigint_to_int16
  br $end_integer_type
)
  local.get $bi
  call $hydra.lib.literals.bigint_to_int32
  br $end_integer_type
)
  local.get $bi
  call $hydra.lib.literals.bigint_to_int64
  br $end_integer_type
)
  local.get $bi
  call $hydra.lib.literals.bigint_to_uint8
  br $end_integer_type
)
  local.get $bi
  call $hydra.lib.literals.bigint_to_uint16
  br $end_integer_type
)
  local.get $bi
  call $hydra.lib.literals.bigint_to_uint32
  br $end_integer_type
)
  local.get $bi
  call $hydra.lib.literals.bigint_to_uint64
  br $end_integer_type
)
)
  (func $hydra.literals.float_value_to_bigfloat (param $arg_0 i32) (result f64)
  (local $bf i32)
  (local $f32 i32)
  (local $f64 i32)
  (block $end_float_value (result i32)
  (block $float64
  (block $float32
  (block $bigfloat
  local.get $arg_0
  br_table $bigfloat $float32 $float64 $float64
)
  local.get $bf
  br $end_float_value
)
  local.get $f32
  call $hydra.lib.literals.float32_to_bigfloat
  br $end_float_value
)
  local.get $f64
  call $hydra.lib.literals.float64_to_bigfloat
  br $end_float_value
)
)
  (func $hydra.literals.integer_value_to_bigint (param $arg_0 i32) (result i32)
  (local $bi i32)
  (local $i16 i32)
  (local $i32 i32)
  (local $i64 i32)
  (local $i8 i32)
  (local $ui16 i32)
  (local $ui32 i32)
  (local $ui64 i32)
  (local $ui8 i32)
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
  local.get $bi
  br $end_integer_value
)
  local.get $i8
  call $hydra.lib.literals.int8_to_bigint
  br $end_integer_value
)
  local.get $i16
  call $hydra.lib.literals.int16_to_bigint
  br $end_integer_value
)
  local.get $i32
  call $hydra.lib.literals.int32_to_bigint
  br $end_integer_value
)
  local.get $i64
  call $hydra.lib.literals.int64_to_bigint
  br $end_integer_value
)
  local.get $ui8
  call $hydra.lib.literals.uint8_to_bigint
  br $end_integer_value
)
  local.get $ui16
  call $hydra.lib.literals.uint16_to_bigint
  br $end_integer_value
)
  local.get $ui32
  call $hydra.lib.literals.uint32_to_bigint
  br $end_integer_value
)
  local.get $ui64
  call $hydra.lib.literals.uint64_to_bigint
  br $end_integer_value
)
)
)
