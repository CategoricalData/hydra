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
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "")
  (global $__bump_ptr (mut i32) i32.const 1024 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.literals.bigfloat_to_float_value" (func $hydra.literals.bigfloat_to_float_value) )
  (export "hydra.literals.bigint_to_integer_value" (func $hydra.literals.bigint_to_integer_value) )
  (export "hydra.literals.float_value_to_bigfloat" (func $hydra.literals.float_value_to_bigfloat) )
  (export "hydra.literals.integer_value_to_bigint" (func $hydra.literals.integer_value_to_bigint) )
  (func $hydra.literals.bigfloat_to_float_value (param $ft i32) (param $bf i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $v i32)
  (block $end_float_type (result i32)
  (block $float64
  (block $float32
  (block $bigfloat
  local.get $ft
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $bigfloat $float32 $float64 $float64
)
  local.get $v
  drop
  i32.const 0
  local.get $bf
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  br $end_float_type
)
  local.get $v
  drop
  i32.const 1
  local.get $bf
  call $hydra.lib.literals.bigfloat_to_float32
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  br $end_float_type
)
  local.get $v
  drop
  i32.const 2
  local.get $bf
  call $hydra.lib.literals.bigfloat_to_float64
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  br $end_float_type
)
)
  (func $hydra.literals.bigint_to_integer_value (param $it i32) (param $bi i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $v i32)
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
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $bigint $int8 $int16 $int32 $int64 $uint8 $uint16 $uint32 $uint64 $uint64
)
  local.get $v
  drop
  i32.const 0
  local.get $bi
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 1
  local.get $bi
  call $hydra.lib.literals.bigint_to_int8
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 2
  local.get $bi
  call $hydra.lib.literals.bigint_to_int16
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 3
  local.get $bi
  call $hydra.lib.literals.bigint_to_int32
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 4
  local.get $bi
  call $hydra.lib.literals.bigint_to_int64
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 5
  local.get $bi
  call $hydra.lib.literals.bigint_to_uint8
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 6
  local.get $bi
  call $hydra.lib.literals.bigint_to_uint16
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 7
  local.get $bi
  call $hydra.lib.literals.bigint_to_uint32
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  br $end_integer_type
)
  local.get $v
  drop
  i32.const 8
  local.get $bi
  call $hydra.lib.literals.bigint_to_uint64
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  br $end_integer_type
)
)
  (func $hydra.literals.float_value_to_bigfloat (param $arg_0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $bf i32)
  (local $f32 i32)
  (local $f64 i32)
  (local $v i32)
  (block $end_float_value (result i32)
  (block $float64
  (block $float32
  (block $bigfloat
  local.get $arg_0
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $bigfloat $float32 $float64 $float64
)
  local.get $v
  drop
  local.get $bf
  br $end_float_value
)
  local.get $v
  drop
  local.get $f32
  call $hydra.lib.literals.float32_to_bigfloat
  br $end_float_value
)
  local.get $v
  drop
  local.get $f64
  call $hydra.lib.literals.float64_to_bigfloat
  br $end_float_value
)
)
  (func $hydra.literals.integer_value_to_bigint (param $arg_0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $bi i32)
  (local $i16 i32)
  (local $i32 i32)
  (local $i64 i32)
  (local $i8 i32)
  (local $ui16 i32)
  (local $ui32 i32)
  (local $ui64 i32)
  (local $ui8 i32)
  (local $v i32)
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
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $bigint $int8 $int16 $int32 $int64 $uint8 $uint16 $uint32 $uint64 $uint64
)
  local.get $v
  drop
  local.get $bi
  br $end_integer_value
)
  local.get $v
  drop
  local.get $i8
  call $hydra.lib.literals.int8_to_bigint
  br $end_integer_value
)
  local.get $v
  drop
  local.get $i16
  call $hydra.lib.literals.int16_to_bigint
  br $end_integer_value
)
  local.get $v
  drop
  local.get $i32
  call $hydra.lib.literals.int32_to_bigint
  br $end_integer_value
)
  local.get $v
  drop
  local.get $i64
  call $hydra.lib.literals.int64_to_bigint
  br $end_integer_value
)
  local.get $v
  drop
  local.get $ui8
  call $hydra.lib.literals.uint8_to_bigint
  br $end_integer_value
)
  local.get $v
  drop
  local.get $ui16
  call $hydra.lib.literals.uint16_to_bigint
  br $end_integer_value
)
  local.get $v
  drop
  local.get $ui32
  call $hydra.lib.literals.uint32_to_bigint
  br $end_integer_value
)
  local.get $v
  drop
  local.get $ui64
  call $hydra.lib.literals.uint64_to_bigint
  br $end_integer_value
)
)
)
