(module
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.find" (func $hydra.lib.lists.find (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip" (func $hydra.lib.lists.zip (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigfloat_to_float64" (func $hydra.lib.literals.bigfloat_to_float64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_decimal" (func $hydra.lib.literals.bigint_to_decimal (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.binary_to_string" (func $hydra.lib.literals.binary_to_string (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.float64_to_decimal" (func $hydra.lib.literals.float64_to_decimal (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.int16_to_bigint" (func $hydra.lib.literals.int16_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.int32_to_bigint" (func $hydra.lib.literals.int32_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.int8_to_bigint" (func $hydra.lib.literals.int8_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_bigfloat" (func $hydra.lib.literals.show_bigfloat (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_bigint" (func $hydra.lib.literals.show_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_float32" (func $hydra.lib.literals.show_float32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_float64" (func $hydra.lib.literals.show_float64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int64" (func $hydra.lib.literals.show_int64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_uint32" (func $hydra.lib.literals.show_uint32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_uint64" (func $hydra.lib.literals.show_uint64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.uint16_to_bigint" (func $hydra.lib.literals.uint16_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.uint8_to_bigint" (func $hydra.lib.literals.uint8_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.or" (func $hydra.lib.logic.or (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_term" (func $hydra.strip.deannotate_term (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\04\00\00\00\2d\30\2e\30\09\00\00\00\2d\49\6e\66\69\6e\69\74\79\06\00\00\00\40\66\69\72\73\74\04\00\00\00\40\6b\65\79\05\00\00\00\40\6c\65\66\74\06\00\00\00\40\72\69\67\68\74\07\00\00\00\40\73\65\63\6f\6e\64\06\00\00\00\40\76\61\6c\75\65\08\00\00\00\49\6e\66\69\6e\69\74\79\26\00\00\00\4a\53\4f\4e\20\63\61\6e\6e\6f\74\20\72\65\70\72\65\73\65\6e\74\20\62\69\67\66\6c\6f\61\74\20\76\61\6c\75\65\3a\20\03\00\00\00\4e\61\4e\14\00\00\00\65\78\70\65\63\74\65\64\20\65\69\74\68\65\72\20\74\65\72\6d\12\00\00\00\65\78\70\65\63\74\65\64\20\6c\69\73\74\20\74\65\72\6d\15\00\00\00\65\78\70\65\63\74\65\64\20\6c\69\74\65\72\61\6c\20\74\65\72\6d\11\00\00\00\65\78\70\65\63\74\65\64\20\6d\61\70\20\74\65\72\6d\13\00\00\00\65\78\70\65\63\74\65\64\20\6d\61\79\62\65\20\74\65\72\6d\26\00\00\00\65\78\70\65\63\74\65\64\20\6d\61\79\62\65\20\74\65\72\6d\20\66\6f\72\20\6f\70\74\69\6f\6e\61\6c\20\66\69\65\6c\64\12\00\00\00\65\78\70\65\63\74\65\64\20\70\61\69\72\20\74\65\72\6d\14\00\00\00\65\78\70\65\63\74\65\64\20\72\65\63\6f\72\64\20\74\65\72\6d\11\00\00\00\65\78\70\65\63\74\65\64\20\73\65\74\20\74\65\72\6d\13\00\00\00\65\78\70\65\63\74\65\64\20\75\6e\69\6f\6e\20\74\65\72\6d\15\00\00\00\65\78\70\65\63\74\65\64\20\77\72\61\70\70\65\64\20\74\65\72\6d\11\00\00\00\75\6e\6b\6e\6f\77\6e\20\76\61\72\69\61\6e\74\3a\20\2c\00\00\00\75\6e\73\75\70\70\6f\72\74\65\64\20\74\65\72\6d\20\76\61\72\69\61\6e\74\20\66\6f\72\20\4a\53\4f\4e\20\65\6e\63\6f\64\69\6e\67\3a\20\24\00\00\00\75\6e\73\75\70\70\6f\72\74\65\64\20\74\79\70\65\20\66\6f\72\20\4a\53\4f\4e\20\65\6e\63\6f\64\69\6e\67\3a\20")
  (global $__bump_ptr (mut i32) i32.const 1552 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.json.encode.encode_float" (func $hydra.json.encode.encode_float) )
  (export "hydra.json.encode.encode_integer" (func $hydra.json.encode.encode_integer) )
  (export "hydra.json.encode.encode_literal" (func $hydra.json.encode.encode_literal) )
  (export "hydra.json.encode.requires_json_string_sentinel" (func $hydra.json.encode.requires_json_string_sentinel) )
  (export "hydra.json.encode.to_json" (func $hydra.json.encode.to_json) )
  (export "hydra.json.encode.to_json_untyped" (func $hydra.json.encode.to_json_untyped) )
  (func $hydra.json.encode.encode_float (param $fv i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $bf i32)
  (local $f i32)
  (local $s i32)
  (local $v i32)
  (block $end_float_value (result i32)
  (block $float64
  (block $float32
  (block $bigfloat
  local.get $fv
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
  call $hydra.lib.literals.show_bigfloat
  local.set $s
  local.get $s
  call $hydra.json.encode.requires_json_string_sentinel
  i32.const 0
  i32.const 1115
  local.get $s
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.strings.cat
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
  i32.const 1
  i32.const 3
  local.get $bf
  call $hydra.lib.literals.bigfloat_to_float64
  call $hydra.lib.literals.float64_to_decimal
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
  call $hydra.lib.logic.if_else
  br $end_float_value
)
  local.get $v
  drop
  i32.const 1
  i32.const 5
  local.get $f
  call $hydra.lib.literals.show_float32
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
  br $end_float_value
)
  local.get $v
  drop
  local.get $f
  call $hydra.lib.literals.show_float64
  local.set $s
  local.get $s
  call $hydra.json.encode.requires_json_string_sentinel
  i32.const 1
  i32.const 5
  local.get $s
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
  i32.const 1
  i32.const 3
  local.get $f
  call $hydra.lib.literals.float64_to_decimal
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
  call $hydra.lib.logic.if_else
  br $end_float_value
)
)
  (func $hydra.json.encode.encode_integer (param $iv i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $bi i32)
  (local $i i32)
  (local $v i32)
  (block $end_integer_value (result i32)
  (block $uint16
  (block $uint8
  (block $int32
  (block $int16
  (block $int8
  (block $uint64
  (block $uint32
  (block $int64
  (block $bigint
  local.get $iv
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $bigint $int64 $uint32 $uint64 $int8 $int16 $int32 $uint8 $uint16 $uint16
)
  local.get $v
  drop
  i32.const 1
  i32.const 5
  local.get $bi
  call $hydra.lib.literals.show_bigint
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
  br $end_integer_value
)
  local.get $v
  drop
  i32.const 1
  i32.const 5
  local.get $i
  call $hydra.lib.literals.show_int64
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
  br $end_integer_value
)
  local.get $v
  drop
  i32.const 1
  i32.const 5
  local.get $i
  call $hydra.lib.literals.show_uint32
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
  br $end_integer_value
)
  local.get $v
  drop
  i32.const 1
  i32.const 5
  local.get $i
  call $hydra.lib.literals.show_uint64
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
  br $end_integer_value
)
  local.get $v
  drop
  i32.const 1
  i32.const 3
  local.get $i
  call $hydra.lib.literals.int8_to_bigint
  call $hydra.lib.literals.bigint_to_decimal
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
  br $end_integer_value
)
  local.get $v
  drop
  i32.const 1
  i32.const 3
  local.get $i
  call $hydra.lib.literals.int16_to_bigint
  call $hydra.lib.literals.bigint_to_decimal
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
  br $end_integer_value
)
  local.get $v
  drop
  i32.const 1
  i32.const 3
  local.get $i
  call $hydra.lib.literals.int32_to_bigint
  call $hydra.lib.literals.bigint_to_decimal
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
  br $end_integer_value
)
  local.get $v
  drop
  i32.const 1
  i32.const 3
  local.get $i
  call $hydra.lib.literals.uint8_to_bigint
  call $hydra.lib.literals.bigint_to_decimal
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
  br $end_integer_value
)
  local.get $v
  drop
  i32.const 1
  i32.const 3
  local.get $i
  call $hydra.lib.literals.uint16_to_bigint
  call $hydra.lib.literals.bigint_to_decimal
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
  br $end_integer_value
)
)
  (func $hydra.json.encode.encode_literal (param $lit i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $d i32)
  (local $f i32)
  (local $i i32)
  (local $s i32)
  (local $v i32)
  (block $end_literal (result i32)
  (block $string
  (block $integer
  (block $float
  (block $decimal
  (block $boolean
  (block $binary
  local.get $lit
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $binary $boolean $decimal $float $integer $string $string
)
  local.get $v
  drop
  i32.const 1
  i32.const 5
  local.get $b
  call $hydra.lib.literals.binary_to_string
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
  br $end_literal
)
  local.get $v
  drop
  i32.const 1
  i32.const 1
  local.get $b
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
  br $end_literal
)
  local.get $v
  drop
  i32.const 1
  i32.const 3
  local.get $d
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
  br $end_literal
)
  local.get $v
  drop
  local.get $f
  call $hydra.json.encode.encode_float
  br $end_literal
)
  local.get $v
  drop
  local.get $i
  call $hydra.json.encode.encode_integer
  br $end_literal
)
  local.get $v
  drop
  i32.const 1
  i32.const 5
  local.get $s
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
  br $end_literal
)
)
  (func $hydra.json.encode.requires_json_string_sentinel (param $s i32) (result i32)
  local.get $s
  i32.const 1157
  call $hydra.lib.equality.equal
  local.get $s
  i32.const 1103
  call $hydra.lib.equality.equal
  local.get $s
  i32.const 1032
  call $hydra.lib.equality.equal
  local.get $s
  i32.const 1024
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.or
  call $hydra.lib.logic.or
  call $hydra.lib.logic.or
)
  (func $hydra.json.encode.to_json (param $types i32) (param $tname i32) (param $typ i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $e i32)
  (local $ef i32)
  (local $ek i32)
  (local $elem_type i32)
  (local $encode_entry i32)
  (local $encode_field_with_type i32)
  (local $encoded i32)
  (local $encoded_first i32)
  (local $encoded_k i32)
  (local $encoded_l i32)
  (local $encoded_pairs i32)
  (local $encoded_r i32)
  (local $encoded_second i32)
  (local $encoded_union i32)
  (local $encoded_v i32)
  (local $entries i32)
  (local $err i32)
  (local $es i32)
  (local $et i32)
  (local $ev i32)
  (local $f i32)
  (local $field i32)
  (local $field_types i32)
  (local $fields i32)
  (local $first i32)
  (local $first_type i32)
  (local $fname i32)
  (local $ft i32)
  (local $fterm i32)
  (local $ftf i32)
  (local $ftype i32)
  (local $ftype_result i32)
  (local $inj i32)
  (local $inner_stripped i32)
  (local $inner_t i32)
  (local $inner_type i32)
  (local $is_nested_maybe i32)
  (local $is_simple_maybe i32)
  (local $it i32)
  (local $k i32)
  (local $key_type i32)
  (local $kv i32)
  (local $l i32)
  (local $left_type i32)
  (local $lit i32)
  (local $looked_up i32)
  (local $m i32)
  (local $mt i32)
  (local $name i32)
  (local $opt i32)
  (local $p i32)
  (local $pairs i32)
  (local $pt i32)
  (local $r i32)
  (local $resolved_type i32)
  (local $results i32)
  (local $right_type i32)
  (local $rt i32)
  (local $second i32)
  (local $second_type i32)
  (local $stripped i32)
  (local $stripped_term i32)
  (local $t i32)
  (local $terms i32)
  (local $v i32)
  (local $val_type i32)
  (local $vals i32)
  (local $vs i32)
  (local $wn i32)
  (local $wt i32)
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $stripped
  local.get $term
  call $hydra.strip.deannotate_term
  local.set $stripped_term
  (block $end_type (result i32)
  (block $variable
  (block $either
  (block $pair
  (block $map
  (block $wrap
  (block $unit
  (block $union
  (block $record
  (block $maybe
  (block $set
  (block $list
  (block $literal
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $literal $list $set $maybe $record $union $unit $wrap $map $pair $either $variable $variable
)
  local.get $v
  drop
  (block $end_term (result i32)
  (block $literal
  local.get $stripped_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $literal $literal
)
  local.get $v
  drop
  local.get $lit
  call $hydra.json.encode.encode_literal
  br $end_term
)
  br $end_type
)
  local.get $v
  drop
  (block $end_term (result i32)
  (block $list
  local.get $stripped_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $list $list
)
  local.get $v
  drop
  local.get $types
  local.get $tname
  local.get $elem_type
  local.get $t
  call $hydra.json.encode.to_json
  local.get $terms
  call $hydra.lib.eithers.map_list
  local.set $results
  i32.const 0
  local.get $vs
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
  local.get $results
  call $hydra.lib.eithers.map
  br $end_term
)
  br $end_type
)
  local.get $v
  drop
  (block $end_term (result i32)
  (block $set
  local.get $stripped_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $set $set
)
  local.get $v
  drop
  local.get $vals
  call $hydra.lib.sets.to_list
  local.set $terms
  local.get $types
  local.get $tname
  local.get $elem_type
  local.get $t
  call $hydra.json.encode.to_json
  local.get $terms
  call $hydra.lib.eithers.map_list
  local.set $results
  i32.const 0
  local.get $vs
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
  local.get $results
  call $hydra.lib.eithers.map
  br $end_term
)
  br $end_type
)
  local.get $v
  drop
  local.get $inner_type
  call $hydra.strip.deannotate_type
  local.set $inner_stripped
  (block $end_type (result i32)
  (block $maybe
  local.get $inner_stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $maybe $maybe
)
  local.get $v
  drop
  i32.const 1
  br $end_type
)
  local.set $is_nested_maybe
  (block $end_term (result i32)
  (block $maybe
  local.get $stripped_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $maybe $maybe
)
  local.get $v
  drop
  i32.const 1
  i32.const 2
  i32.const 0
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
  local.get $types
  local.get $tname
  local.get $inner_type
  local.get $v
  call $hydra.json.encode.to_json
  local.set $encoded
  local.get $is_nested_maybe
  i32.const 0
  local.get $ev
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
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
  local.get $encoded
  call $hydra.lib.eithers.map
  local.get $encoded
  call $hydra.lib.logic.if_else
  local.get $opt
  call $hydra.lib.maybes.maybe
  br $end_term
)
  br $end_type
)
  local.get $v
  drop
  (block $end_term (result i32)
  (block $record
  local.get $stripped_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $record $record
)
  local.get $v
  drop
  (block $end_type (result i32)
  (block $maybe
  local.get $ftype
  call $hydra.strip.deannotate_type
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $maybe $maybe
)
  local.get $v
  drop
  (block $end_type (result i32)
  (block $maybe
  local.get $inner_t
  call $hydra.strip.deannotate_type
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $maybe $maybe
)
  local.get $v
  drop
  i32.const 0
  br $end_type
)
  br $end_type
)
  local.set $is_simple_maybe
  local.get $f
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $fname
  local.get $f
  i32.load offset=4
  local.set $fterm
  local.get $ft
  i32.load offset=4
  local.set $ftype
  local.get $ftype
  drop
  local.get $is_simple_maybe
  drop
  i32.const 0
  (block $end_term (result i32)
  (block $maybe
  local.get $fterm
  call $hydra.strip.deannotate_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $maybe $maybe
)
  local.get $v
  drop
  i32.const 1
  i32.const 0
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
  (block $end_type (result i32)
  (block $maybe
  local.get $ftype
  call $hydra.strip.deannotate_type
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $maybe $maybe
)
  local.get $v
  drop
  local.get $it
  br $end_type
)
  local.set $inner_type
  local.get $types
  local.get $tname
  local.get $inner_type
  local.get $v
  call $hydra.json.encode.to_json
  local.set $encoded
  local.get $fname
  local.get $ev
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
  local.get $encoded
  call $hydra.lib.eithers.map
  local.get $opt
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.get $types
  local.get $tname
  local.get $ftype
  local.get $fterm
  call $hydra.json.encode.to_json
  local.set $encoded
  local.get $fname
  local.get $ev
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
  local.get $encoded
  call $hydra.lib.eithers.map
  call $hydra.lib.logic.if_else
  local.set $encode_field_with_type
  local.get $rt
  local.set $field_types
  local.get $r
  i32.load offset=4
  local.set $fields
  local.get $ftf
  call $hydra.lib.pairs.first
  drop
  local.get $ftf
  call $hydra.lib.pairs.second
  drop
  local.get $encode_field_with_type
  drop
  i32.const 0
  local.get $field_types
  local.get $fields
  call $hydra.lib.lists.zip
  call $hydra.lib.eithers.map_list
  local.set $encoded_pairs
  i32.const 4
  local.get $pairs
  call $hydra.lib.maybes.cat
  call $hydra.lib.maps.from_list
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
  local.get $encoded_pairs
  call $hydra.lib.eithers.map
  br $end_term
)
  br $end_type
)
  local.get $v
  drop
  (block $end_term (result i32)
  (block $inject
  local.get $stripped_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $inject $inject
)
  local.get $v
  drop
  local.get $inj
  i32.load offset=4
  local.set $field
  local.get $field
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $fname
  local.get $field
  i32.load offset=4
  local.set $fterm
  i32.const 0
  i32.const 1436
  local.get $fname
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.strings.cat
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
  i32.const 1
  local.get $ft
  i32.load offset=4
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
  local.get $ft
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.get $fname
  call $hydra.lib.equality.equal
  local.get $rt
  call $hydra.lib.lists.find
  call $hydra.lib.maybes.maybe
  local.set $ftype_result
  i32.const 0
  local.get $err
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
  local.get $types
  local.get $tname
  local.get $ftype
  local.get $fterm
  call $hydra.json.encode.to_json
  local.set $encoded_union
  i32.const 4
  local.get $fname
  local.get $v
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.maps.from_list
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
  local.get $encoded_union
  call $hydra.lib.eithers.map
  local.get $ftype_result
  call $hydra.lib.eithers.either
  br $end_term
)
  br $end_type
)
  local.get $v
  drop
  i32.const 1
  i32.const 4
  i32.const 0
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
  br $end_type
)
  local.get $v
  drop
  (block $end_term (result i32)
  (block $wrap
  local.get $stripped_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $wrap $wrap
)
  local.get $v
  drop
  local.get $types
  local.get $tname
  local.get $wn
  local.get $wt
  i32.load offset=4
  call $hydra.json.encode.to_json
  br $end_term
)
  br $end_type
)
  local.get $v
  drop
  local.get $mt
  i32.load
  local.set $key_type
  local.get $mt
  i32.load offset=4
  local.set $val_type
  (block $end_term (result i32)
  (block $map
  local.get $stripped_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $map $map
)
  local.get $v
  drop
  local.get $kv
  call $hydra.lib.pairs.first
  local.set $k
  local.get $kv
  call $hydra.lib.pairs.second
  local.set $v
  local.get $types
  local.get $tname
  local.get $key_type
  local.get $k
  call $hydra.json.encode.to_json
  local.set $encoded_k
  local.get $types
  local.get $tname
  local.get $val_type
  local.get $v
  call $hydra.json.encode.to_json
  local.set $encoded_v
  i32.const 0
  local.get $err
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
  i32.const 4
  i32.const 1055
  local.get $ek
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
  i32.const 1093
  local.get $ev
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
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.maps.from_list
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
  local.get $encoded_v
  call $hydra.lib.eithers.map
  local.get $encoded_k
  call $hydra.lib.eithers.either
  local.set $encode_entry
  local.get $encode_entry
  local.get $m
  call $hydra.lib.maps.to_list
  call $hydra.lib.eithers.map_list
  local.set $entries
  i32.const 0
  local.get $es
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
  local.get $entries
  call $hydra.lib.eithers.map
  br $end_term
)
  br $end_type
)
  local.get $v
  drop
  local.get $pt
  i32.load
  local.set $first_type
  local.get $pt
  i32.load offset=4
  local.set $second_type
  (block $end_term (result i32)
  (block $pair
  local.get $stripped_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $pair $pair
)
  local.get $v
  drop
  local.get $p
  call $hydra.lib.pairs.first
  local.set $first
  local.get $p
  call $hydra.lib.pairs.second
  local.set $second
  local.get $types
  local.get $tname
  local.get $first_type
  local.get $first
  call $hydra.json.encode.to_json
  local.set $encoded_first
  local.get $types
  local.get $tname
  local.get $second_type
  local.get $second
  call $hydra.json.encode.to_json
  local.set $encoded_second
  i32.const 0
  local.get $err
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
  i32.const 4
  i32.const 1045
  local.get $ef
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
  i32.const 1082
  local.get $es
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
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.maps.from_list
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
  local.get $encoded_second
  call $hydra.lib.eithers.map
  local.get $encoded_first
  call $hydra.lib.eithers.either
  br $end_term
)
  br $end_type
)
  local.get $v
  drop
  local.get $et
  i32.load
  local.set $left_type
  local.get $et
  i32.load offset=4
  local.set $right_type
  (block $end_term (result i32)
  (block $either
  local.get $stripped_term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $either $either
)
  local.get $v
  drop
  local.get $types
  local.get $tname
  local.get $left_type
  local.get $l
  call $hydra.json.encode.to_json
  local.set $encoded_l
  i32.const 4
  i32.const 1063
  local.get $v
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.maps.from_list
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
  local.get $encoded_l
  call $hydra.lib.eithers.map
  local.get $types
  local.get $tname
  local.get $right_type
  local.get $r
  call $hydra.json.encode.to_json
  local.set $encoded_r
  i32.const 4
  i32.const 1072
  local.get $v
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.maps.from_list
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
  local.get $encoded_r
  call $hydra.lib.eithers.map
  local.get $e
  call $hydra.lib.eithers.either
  br $end_term
)
  br $end_type
)
  local.get $v
  drop
  local.get $name
  local.get $types
  call $hydra.lib.maps.lookup
  local.set $looked_up
  local.get $term
  call $hydra.json.encode.to_json_untyped
  local.get $types
  local.get $name
  local.get $resolved_type
  local.get $term
  call $hydra.json.encode.to_json
  local.get $looked_up
  call $hydra.lib.maybes.maybe
  br $end_type
)
)
  (func $hydra.json.encode.to_json_untyped (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $e i32)
  (local $ef i32)
  (local $ek i32)
  (local $encode_entry i32)
  (local $encode_field i32)
  (local $encoded i32)
  (local $encoded_field i32)
  (local $encoded_fields i32)
  (local $encoded_first i32)
  (local $encoded_k i32)
  (local $encoded_l i32)
  (local $encoded_maybe i32)
  (local $encoded_r i32)
  (local $encoded_second i32)
  (local $encoded_union i32)
  (local $encoded_v i32)
  (local $entries i32)
  (local $err i32)
  (local $es i32)
  (local $ev i32)
  (local $f i32)
  (local $field i32)
  (local $fields i32)
  (local $first i32)
  (local $fname i32)
  (local $fs i32)
  (local $fterm i32)
  (local $inj i32)
  (local $k i32)
  (local $kv i32)
  (local $l i32)
  (local $lit i32)
  (local $m i32)
  (local $opt i32)
  (local $p i32)
  (local $r i32)
  (local $results i32)
  (local $second i32)
  (local $stripped i32)
  (local $t i32)
  (local $terms i32)
  (local $v i32)
  (local $vals i32)
  (local $vs i32)
  (local $wt i32)
  local.get $term
  call $hydra.strip.deannotate_term
  local.set $stripped
  (block $end_term (result i32)
  (block $either
  (block $pair
  (block $map
  (block $wrap
  (block $unit
  (block $inject
  (block $record
  (block $maybe
  (block $set
  (block $list
  (block $literal
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $literal $list $set $maybe $record $inject $unit $wrap $map $pair $either $either
)
  local.get $v
  drop
  local.get $lit
  call $hydra.json.encode.encode_literal
  br $end_term
)
  local.get $v
  drop
  local.get $t
  call $hydra.json.encode.to_json_untyped
  local.get $terms
  call $hydra.lib.eithers.map_list
  local.set $results
  i32.const 0
  local.get $vs
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
  local.get $results
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $v
  drop
  local.get $vals
  call $hydra.lib.sets.to_list
  local.set $terms
  local.get $t
  call $hydra.json.encode.to_json_untyped
  local.get $terms
  call $hydra.lib.eithers.map_list
  local.set $results
  i32.const 0
  local.get $vs
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
  local.get $results
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $v
  drop
  i32.const 1
  i32.const 2
  i32.const 0
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
  local.get $v
  call $hydra.json.encode.to_json_untyped
  local.set $encoded_maybe
  i32.const 0
  local.get $encoded
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
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
  local.get $encoded_maybe
  call $hydra.lib.eithers.map
  local.get $opt
  call $hydra.lib.maybes.maybe
  br $end_term
)
  local.get $v
  drop
  local.get $f
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $fname
  local.get $f
  i32.load offset=4
  local.set $fterm
  local.get $fterm
  call $hydra.json.encode.to_json_untyped
  local.set $encoded_field
  local.get $fname
  local.get $v
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
  local.get $encoded_field
  call $hydra.lib.eithers.map
  local.set $encode_field
  local.get $r
  i32.load offset=4
  local.set $fields
  local.get $encode_field
  local.get $fields
  call $hydra.lib.eithers.map_list
  local.set $encoded_fields
  i32.const 4
  local.get $fs
  call $hydra.lib.maps.from_list
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
  local.get $encoded_fields
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $v
  drop
  local.get $inj
  i32.load offset=4
  local.set $field
  local.get $field
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.set $fname
  local.get $field
  i32.load offset=4
  local.set $fterm
  local.get $fterm
  call $hydra.json.encode.to_json_untyped
  local.set $encoded_union
  i32.const 4
  local.get $fname
  local.get $v
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.maps.from_list
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
  local.get $encoded_union
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $v
  drop
  i32.const 1
  i32.const 4
  i32.const 0
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
  br $end_term
)
  local.get $v
  drop
  local.get $wt
  i32.load offset=4
  call $hydra.json.encode.to_json_untyped
  br $end_term
)
  local.get $v
  drop
  local.get $kv
  call $hydra.lib.pairs.first
  local.set $k
  local.get $kv
  call $hydra.lib.pairs.second
  local.set $v
  local.get $k
  call $hydra.json.encode.to_json_untyped
  local.set $encoded_k
  local.get $v
  call $hydra.json.encode.to_json_untyped
  local.set $encoded_v
  i32.const 0
  local.get $err
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
  i32.const 4
  i32.const 1055
  local.get $ek
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
  i32.const 1093
  local.get $ev
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
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.maps.from_list
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
  local.get $encoded_v
  call $hydra.lib.eithers.map
  local.get $encoded_k
  call $hydra.lib.eithers.either
  local.set $encode_entry
  local.get $encode_entry
  local.get $m
  call $hydra.lib.maps.to_list
  call $hydra.lib.eithers.map_list
  local.set $entries
  i32.const 0
  local.get $es
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
  local.get $entries
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $v
  drop
  local.get $p
  call $hydra.lib.pairs.first
  local.set $first
  local.get $p
  call $hydra.lib.pairs.second
  local.set $second
  local.get $first
  call $hydra.json.encode.to_json_untyped
  local.set $encoded_first
  local.get $second
  call $hydra.json.encode.to_json_untyped
  local.set $encoded_second
  i32.const 0
  local.get $err
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
  i32.const 4
  i32.const 1045
  local.get $ef
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
  i32.const 1082
  local.get $es
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
  i32.const 12
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 2
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=8
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.maps.from_list
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
  local.get $encoded_second
  call $hydra.lib.eithers.map
  local.get $encoded_first
  call $hydra.lib.eithers.either
  br $end_term
)
  local.get $v
  drop
  local.get $l
  call $hydra.json.encode.to_json_untyped
  local.set $encoded_l
  i32.const 4
  i32.const 1063
  local.get $v
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.maps.from_list
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
  local.get $encoded_l
  call $hydra.lib.eithers.map
  local.get $r
  call $hydra.json.encode.to_json_untyped
  local.set $encoded_r
  i32.const 4
  i32.const 1072
  local.get $v
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 1
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.get $__rec_ptr
  call $hydra.lib.maps.from_list
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
  local.get $encoded_r
  call $hydra.lib.eithers.map
  local.get $e
  call $hydra.lib.eithers.either
  br $end_term
)
)
)
