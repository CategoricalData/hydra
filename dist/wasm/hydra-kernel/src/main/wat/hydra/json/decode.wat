(module
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.find" (func $hydra.lib.lists.find (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.maybe_head" (func $hydra.lib.lists.maybe_head (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_int16" (func $hydra.lib.literals.bigint_to_int16 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_int32" (func $hydra.lib.literals.bigint_to_int32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_int8" (func $hydra.lib.literals.bigint_to_int8 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_uint16" (func $hydra.lib.literals.bigint_to_uint16 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_uint8" (func $hydra.lib.literals.bigint_to_uint8 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.decimal_to_bigint" (func $hydra.lib.literals.decimal_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.decimal_to_float64" (func $hydra.lib.literals.decimal_to_float64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.float64_to_bigfloat" (func $hydra.lib.literals.float64_to_bigfloat (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.read_bigint" (func $hydra.lib.literals.read_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.read_float32" (func $hydra.lib.literals.read_float32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.read_float64" (func $hydra.lib.literals.read_float64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.read_int64" (func $hydra.lib.literals.read_int64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.read_uint32" (func $hydra.lib.literals.read_uint32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.read_uint64" (func $hydra.lib.literals.read_uint64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.string_to_binary" (func $hydra.lib.literals.string_to_binary (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.or" (func $hydra.lib.logic.or (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.keys" (func $hydra.lib.maps.keys (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.from_maybe" (func $hydra.lib.maybes.from_maybe (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\04\00\00\00\2d\30\2e\30\09\00\00\00\2d\49\6e\66\69\6e\69\74\79\06\00\00\00\40\66\69\72\73\74\04\00\00\00\40\6b\65\79\05\00\00\00\40\6c\65\66\74\06\00\00\00\40\72\69\67\68\74\07\00\00\00\40\73\65\63\6f\6e\64\06\00\00\00\40\76\61\6c\75\65\08\00\00\00\49\6e\66\69\6e\69\74\79\03\00\00\00\4e\61\4e\22\00\00\00\65\78\70\65\63\74\65\64\20\40\6c\65\66\74\20\6f\72\20\40\72\69\67\68\74\20\69\6e\20\45\69\74\68\65\72\0e\00\00\00\65\78\70\65\63\74\65\64\20\61\72\72\61\79\10\00\00\00\65\78\70\65\63\74\65\64\20\62\6f\6f\6c\65\61\6e\36\00\00\00\65\78\70\65\63\74\65\64\20\6e\75\6c\6c\20\6f\72\20\73\69\6e\67\6c\65\2d\65\6c\65\6d\65\6e\74\20\61\72\72\61\79\20\66\6f\72\20\6e\65\73\74\65\64\20\4d\61\79\62\65\0f\00\00\00\65\78\70\65\63\74\65\64\20\6e\75\6d\62\65\72\1b\00\00\00\65\78\70\65\63\74\65\64\20\6e\75\6d\62\65\72\20\66\6f\72\20\64\65\63\69\6d\61\6c\34\00\00\00\65\78\70\65\63\74\65\64\20\6e\75\6d\62\65\72\20\6f\72\20\73\70\65\63\69\61\6c\20\66\6c\6f\61\74\20\73\74\72\69\6e\67\20\66\6f\72\20\62\69\67\66\6c\6f\61\74\33\00\00\00\65\78\70\65\63\74\65\64\20\6e\75\6d\62\65\72\20\6f\72\20\73\70\65\63\69\61\6c\20\66\6c\6f\61\74\20\73\74\72\69\6e\67\20\66\6f\72\20\66\6c\6f\61\74\36\34\0f\00\00\00\65\78\70\65\63\74\65\64\20\6f\62\6a\65\63\74\26\00\00\00\65\78\70\65\63\74\65\64\20\73\69\6e\67\6c\65\2d\65\6c\65\6d\65\6e\74\20\61\72\72\61\79\20\66\6f\72\20\4a\75\73\74\24\00\00\00\65\78\70\65\63\74\65\64\20\73\69\6e\67\6c\65\2d\6b\65\79\20\6f\62\6a\65\63\74\20\66\6f\72\20\75\6e\69\6f\6e\0f\00\00\00\65\78\70\65\63\74\65\64\20\73\74\72\69\6e\67\1b\00\00\00\69\6e\76\61\6c\69\64\20\62\69\67\66\6c\6f\61\74\20\73\65\6e\74\69\6e\65\6c\3a\20\10\00\00\00\69\6e\76\61\6c\69\64\20\62\69\67\69\6e\74\3a\20\11\00\00\00\69\6e\76\61\6c\69\64\20\66\6c\6f\61\74\33\32\3a\20\1a\00\00\00\69\6e\76\61\6c\69\64\20\66\6c\6f\61\74\36\34\20\73\65\6e\74\69\6e\65\6c\3a\20\0f\00\00\00\69\6e\76\61\6c\69\64\20\69\6e\74\36\34\3a\20\10\00\00\00\69\6e\76\61\6c\69\64\20\75\69\6e\74\33\32\3a\20\10\00\00\00\69\6e\76\61\6c\69\64\20\75\69\6e\74\36\34\3a\20\16\00\00\00\6d\69\73\73\69\6e\67\20\40\66\69\72\73\74\20\69\6e\20\70\61\69\72\19\00\00\00\6d\69\73\73\69\6e\67\20\40\6b\65\79\20\69\6e\20\6d\61\70\20\65\6e\74\72\79\17\00\00\00\6d\69\73\73\69\6e\67\20\40\73\65\63\6f\6e\64\20\69\6e\20\70\61\69\72\1b\00\00\00\6d\69\73\73\69\6e\67\20\40\76\61\6c\75\65\20\69\6e\20\6d\61\70\20\65\6e\74\72\79\17\00\00\00\75\6e\6b\6e\6f\77\6e\20\74\79\70\65\20\76\61\72\69\61\62\6c\65\3a\20\11\00\00\00\75\6e\6b\6e\6f\77\6e\20\76\61\72\69\61\6e\74\3a\20\24\00\00\00\75\6e\73\75\70\70\6f\72\74\65\64\20\74\79\70\65\20\66\6f\72\20\4a\53\4f\4e\20\64\65\63\6f\64\69\6e\67\3a\20")
  (global $__bump_ptr (mut i32) i32.const 1904 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.json.decode.decode_float" (func $hydra.json.decode.decode_float) )
  (export "hydra.json.decode.decode_integer" (func $hydra.json.decode.decode_integer) )
  (export "hydra.json.decode.decode_literal" (func $hydra.json.decode.decode_literal) )
  (export "hydra.json.decode.expect_array" (func $hydra.json.decode.expect_array) )
  (export "hydra.json.decode.expect_number" (func $hydra.json.decode.expect_number) )
  (export "hydra.json.decode.expect_object" (func $hydra.json.decode.expect_object) )
  (export "hydra.json.decode.expect_string" (func $hydra.json.decode.expect_string) )
  (export "hydra.json.decode.from_json" (func $hydra.json.decode.from_json) )
  (export "hydra.json.decode.parse_special_float" (func $hydra.json.decode.parse_special_float) )
  (func $hydra.json.decode.decode_float (param $ft i32) (param $value i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $n i32)
  (local $parsed i32)
  (local $s i32)
  (local $str_result i32)
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
  (block $end_value (result i32)
  (block $string
  (block $number
  local.get $value
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $number $string $string
)
  local.get $v
  drop
  i32.const 1
  i32.const 8
  i32.const 3
  i32.const 0
  local.get $n
  call $hydra.lib.literals.decimal_to_float64
  call $hydra.lib.literals.float64_to_bigfloat
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
  br $end_value
)
  local.get $v
  drop
  i32.const 0
  i32.const 1537
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
  i32.const 8
  i32.const 3
  i32.const 0
  local.get $v
  call $hydra.lib.literals.float64_to_bigfloat
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
  local.get $s
  call $hydra.json.decode.parse_special_float
  call $hydra.lib.maybes.maybe
  br $end_value
)
  br $end_float_type
)
  local.get $v
  drop
  local.get $value
  call $hydra.json.decode.expect_string
  local.set $str_result
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
  local.get $s
  call $hydra.lib.literals.read_float32
  local.set $parsed
  i32.const 0
  i32.const 1588
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
  i32.const 8
  i32.const 3
  i32.const 1
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
  local.get $parsed
  call $hydra.lib.maybes.maybe
  local.get $str_result
  call $hydra.lib.eithers.either
  br $end_float_type
)
  local.get $v
  drop
  (block $end_value (result i32)
  (block $string
  (block $number
  local.get $value
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $number $string $string
)
  local.get $v
  drop
  i32.const 1
  i32.const 8
  i32.const 3
  i32.const 2
  local.get $n
  call $hydra.lib.literals.decimal_to_float64
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
  br $end_value
)
  local.get $v
  drop
  i32.const 0
  i32.const 1609
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
  i32.const 8
  i32.const 3
  i32.const 2
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
  local.get $s
  call $hydra.json.decode.parse_special_float
  call $hydra.lib.maybes.maybe
  br $end_value
)
  br $end_float_type
)
)
  (func $hydra.json.decode.decode_integer (param $it i32) (param $value i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $err i32)
  (local $n i32)
  (local $num_result i32)
  (local $parsed i32)
  (local $s i32)
  (local $str_result i32)
  (local $v i32)
  (block $end_integer_type (result i32)
  (block $uint16
  (block $uint8
  (block $int32
  (block $int16
  (block $int8
  (block $uint64
  (block $uint32
  (block $int64
  (block $bigint
  local.get $it
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
  local.get $value
  call $hydra.json.decode.expect_string
  local.set $str_result
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
  local.get $s
  call $hydra.lib.literals.read_bigint
  local.set $parsed
  i32.const 0
  i32.const 1568
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
  i32.const 8
  i32.const 4
  i32.const 0
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
  local.get $parsed
  call $hydra.lib.maybes.maybe
  local.get $str_result
  call $hydra.lib.eithers.either
  br $end_integer_type
)
  local.get $v
  drop
  local.get $value
  call $hydra.json.decode.expect_string
  local.set $str_result
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
  local.get $s
  call $hydra.lib.literals.read_int64
  local.set $parsed
  i32.const 0
  i32.const 1639
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
  i32.const 8
  i32.const 4
  i32.const 4
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
  local.get $parsed
  call $hydra.lib.maybes.maybe
  local.get $str_result
  call $hydra.lib.eithers.either
  br $end_integer_type
)
  local.get $v
  drop
  local.get $value
  call $hydra.json.decode.expect_string
  local.set $str_result
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
  local.get $s
  call $hydra.lib.literals.read_uint32
  local.set $parsed
  i32.const 0
  i32.const 1658
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
  i32.const 8
  i32.const 4
  i32.const 7
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
  local.get $parsed
  call $hydra.lib.maybes.maybe
  local.get $str_result
  call $hydra.lib.eithers.either
  br $end_integer_type
)
  local.get $v
  drop
  local.get $value
  call $hydra.json.decode.expect_string
  local.set $str_result
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
  local.get $s
  call $hydra.lib.literals.read_uint64
  local.set $parsed
  i32.const 0
  i32.const 1678
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
  i32.const 8
  i32.const 4
  i32.const 8
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
  local.get $parsed
  call $hydra.lib.maybes.maybe
  local.get $str_result
  call $hydra.lib.eithers.either
  br $end_integer_type
)
  local.get $v
  drop
  local.get $value
  call $hydra.json.decode.expect_number
  local.set $num_result
  i32.const 8
  i32.const 4
  i32.const 1
  local.get $n
  call $hydra.lib.literals.decimal_to_bigint
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
  local.get $num_result
  call $hydra.lib.eithers.map
  br $end_integer_type
)
  local.get $v
  drop
  local.get $value
  call $hydra.json.decode.expect_number
  local.set $num_result
  i32.const 8
  i32.const 4
  i32.const 2
  local.get $n
  call $hydra.lib.literals.decimal_to_bigint
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
  local.get $num_result
  call $hydra.lib.eithers.map
  br $end_integer_type
)
  local.get $v
  drop
  local.get $value
  call $hydra.json.decode.expect_number
  local.set $num_result
  i32.const 8
  i32.const 4
  i32.const 3
  local.get $n
  call $hydra.lib.literals.decimal_to_bigint
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
  local.get $num_result
  call $hydra.lib.eithers.map
  br $end_integer_type
)
  local.get $v
  drop
  local.get $value
  call $hydra.json.decode.expect_number
  local.set $num_result
  i32.const 8
  i32.const 4
  i32.const 5
  local.get $n
  call $hydra.lib.literals.decimal_to_bigint
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
  local.get $num_result
  call $hydra.lib.eithers.map
  br $end_integer_type
)
  local.get $v
  drop
  local.get $value
  call $hydra.json.decode.expect_number
  local.set $num_result
  i32.const 8
  i32.const 4
  i32.const 6
  local.get $n
  call $hydra.lib.literals.decimal_to_bigint
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
  local.get $num_result
  call $hydra.lib.eithers.map
  br $end_integer_type
)
)
  (func $hydra.json.decode.decode_literal (param $lt i32) (param $value i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $ft i32)
  (local $it i32)
  (local $n i32)
  (local $s i32)
  (local $str_result i32)
  (local $v i32)
  (block $end_literal_type (result i32)
  (block $string
  (block $integer
  (block $float
  (block $decimal
  (block $boolean
  (block $binary
  local.get $lt
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
  local.get $value
  call $hydra.json.decode.expect_string
  local.set $str_result
  i32.const 8
  i32.const 0
  local.get $s
  call $hydra.lib.literals.string_to_binary
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
  local.get $str_result
  call $hydra.lib.eithers.map
  br $end_literal_type
)
  local.get $v
  drop
  (block $end_value (result i32)
  (block $boolean
  local.get $value
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $boolean $boolean
)
  local.get $v
  drop
  i32.const 1
  i32.const 8
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
  br $end_value
)
  br $end_literal_type
)
  local.get $v
  drop
  (block $end_value (result i32)
  (block $number
  local.get $value
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $number $number
)
  local.get $v
  drop
  i32.const 1
  i32.const 8
  i32.const 2
  local.get $n
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
  br $end_value
)
  br $end_literal_type
)
  local.get $v
  drop
  local.get $ft
  local.get $value
  call $hydra.json.decode.decode_float
  br $end_literal_type
)
  local.get $v
  drop
  local.get $it
  local.get $value
  call $hydra.json.decode.decode_integer
  br $end_literal_type
)
  local.get $v
  drop
  local.get $value
  call $hydra.json.decode.expect_string
  local.set $str_result
  i32.const 8
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
  local.get $str_result
  call $hydra.lib.eithers.map
  br $end_literal_type
)
)
  (func $hydra.json.decode.expect_array (param $value i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $arr i32)
  (local $v i32)
  (block $end_value (result i32)
  (block $array
  local.get $value
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $array $array
)
  local.get $v
  drop
  i32.const 1
  local.get $arr
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
  br $end_value
)
)
  (func $hydra.json.decode.expect_number (param $value i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $n i32)
  (local $v i32)
  (block $end_value (result i32)
  (block $number
  local.get $value
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $number $number
)
  local.get $v
  drop
  i32.const 1
  local.get $n
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
  br $end_value
)
)
  (func $hydra.json.decode.expect_object (param $value i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $obj i32)
  (local $v i32)
  (block $end_value (result i32)
  (block $object
  local.get $value
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $object $object
)
  local.get $v
  drop
  i32.const 1
  local.get $obj
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
  br $end_value
)
)
  (func $hydra.json.decode.expect_string (param $value i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $s i32)
  (local $v i32)
  (block $end_value (result i32)
  (block $string
  local.get $value
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $string $string
)
  local.get $v
  drop
  i32.const 1
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
  br $end_value
)
)
  (func $hydra.json.decode.from_json (param $types i32) (param $tname i32) (param $typ i32) (param $value i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $arr i32)
  (local $arr_result i32)
  (local $decode_elem i32)
  (local $decode_entry i32)
  (local $decode_field i32)
  (local $decode_just i32)
  (local $decode_maybe_array i32)
  (local $decode_single_key i32)
  (local $decode_variant i32)
  (local $decoded i32)
  (local $decoded_fields i32)
  (local $decoded_first i32)
  (local $decoded_key i32)
  (local $decoded_second i32)
  (local $decoded_val i32)
  (local $default_val i32)
  (local $elem_type i32)
  (local $elems i32)
  (local $entries i32)
  (local $entry_json i32)
  (local $entry_obj i32)
  (local $err i32)
  (local $es i32)
  (local $et i32)
  (local $f i32)
  (local $find_and_decode i32)
  (local $first_json i32)
  (local $first_type i32)
  (local $first_val i32)
  (local $fj i32)
  (local $fname i32)
  (local $fs i32)
  (local $ft i32)
  (local $fts i32)
  (local $ftype i32)
  (local $inner_stripped i32)
  (local $inner_type i32)
  (local $is_nested_maybe i32)
  (local $json_val i32)
  (local $k i32)
  (local $key i32)
  (local $key_json i32)
  (local $key_type i32)
  (local $kj i32)
  (local $left_json i32)
  (local $left_type i32)
  (local $len i32)
  (local $lj i32)
  (local $looked_up i32)
  (local $lt i32)
  (local $mt i32)
  (local $mval i32)
  (local $name i32)
  (local $obj i32)
  (local $obj_result i32)
  (local $process_union i32)
  (local $pt i32)
  (local $resolved_type i32)
  (local $right_json i32)
  (local $right_type i32)
  (local $rj i32)
  (local $rt i32)
  (local $s i32)
  (local $second_json i32)
  (local $second_type i32)
  (local $sj i32)
  (local $stripped i32)
  (local $ts i32)
  (local $v i32)
  (local $val i32)
  (local $val_json i32)
  (local $val_type i32)
  (local $vj i32)
  (local $wn i32)
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $stripped
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
  local.get $lt
  local.get $value
  call $hydra.json.decode.decode_literal
  br $end_type
)
  local.get $v
  drop
  local.get $types
  local.get $tname
  local.get $elem_type
  local.get $v
  call $hydra.json.decode.from_json
  local.set $decode_elem
  local.get $value
  call $hydra.json.decode.expect_array
  local.set $arr_result
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
  local.get $decode_elem
  local.get $arr
  call $hydra.lib.eithers.map_list
  local.set $decoded
  i32.const 7
  local.get $ts
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
  local.get $decoded
  call $hydra.lib.eithers.map
  local.get $arr_result
  call $hydra.lib.eithers.either
  br $end_type
)
  local.get $v
  drop
  local.get $types
  local.get $tname
  local.get $elem_type
  local.get $v
  call $hydra.json.decode.from_json
  local.set $decode_elem
  local.get $value
  call $hydra.json.decode.expect_array
  local.set $arr_result
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
  local.get $decode_elem
  local.get $arr
  call $hydra.lib.eithers.map_list
  local.set $decoded
  i32.const 14
  local.get $elems
  call $hydra.lib.sets.from_list
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
  local.get $decoded
  call $hydra.lib.eithers.map
  local.get $arr_result
  call $hydra.lib.eithers.either
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
  local.get $is_nested_maybe
  i32.const 0
  i32.const 1436
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
  i32.const 10
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
  local.get $types
  local.get $tname
  local.get $inner_type
  local.get $first_val
  call $hydra.json.decode.from_json
  call $hydra.lib.eithers.map
  local.get $arr
  call $hydra.lib.lists.maybe_head
  call $hydra.lib.maybes.maybe
  local.set $decode_just
  local.get $arr
  call $hydra.lib.lists.length
  local.set $len
  local.get $len
  i32.const 0
  call $hydra.lib.equality.equal
  i32.const 1
  i32.const 10
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
  local.get $len
  i32.const 1
  call $hydra.lib.equality.equal
  local.get $arr
  drop
  local.get $decode_just
  drop
  i32.const 0
  i32.const 0
  i32.const 1436
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
  call $hydra.lib.logic.if_else
  local.set $decode_maybe_array
  (block $end_value (result i32)
  (block $array
  (block $null
  local.get $value
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $null $array $array
)
  local.get $v
  drop
  i32.const 1
  i32.const 10
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
  br $end_value
)
  local.get $v
  drop
  local.get $arr
  drop
  local.get $decode_maybe_array
  drop
  i32.const 0
  br $end_value
)
  (block $end_value (result i32)
  (block $null
  local.get $value
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $null $null
)
  local.get $v
  drop
  i32.const 1
  i32.const 10
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
  br $end_value
)
  call $hydra.lib.logic.if_else
  br $end_type
)
  local.get $v
  drop
  local.get $value
  call $hydra.json.decode.expect_object
  local.set $obj_result
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
  local.get $ft
  i32.load
  local.set $fname
  local.get $ft
  i32.load offset=4
  local.set $ftype
  local.get $fname
  drop
  i32.const 0
  drop
  i32.const 0
  local.get $obj
  call $hydra.lib.maps.lookup
  local.set $mval
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
  local.set $default_val
  local.get $default_val
  local.get $mval
  call $hydra.lib.maybes.from_maybe
  local.set $json_val
  local.get $types
  local.get $tname
  local.get $ftype
  local.get $json_val
  call $hydra.json.decode.from_json
  local.set $decoded
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
  local.get $decoded
  call $hydra.lib.eithers.map
  local.set $decode_field
  local.get $decode_field
  local.get $rt
  call $hydra.lib.eithers.map_list
  local.set $decoded_fields
  i32.const 13
  local.get $tname
  local.get $fs
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
  local.get $decoded_fields
  call $hydra.lib.eithers.map
  local.get $obj_result
  call $hydra.lib.eithers.either
  br $end_type
)
  local.get $v
  drop
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
  local.get $val
  call $hydra.lib.maybes.from_maybe
  local.set $json_val
  local.get $types
  local.get $tname
  local.get $ftype
  local.get $json_val
  call $hydra.json.decode.from_json
  local.set $decoded
  i32.const 4
  local.get $tname
  local.get $key
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
  local.get $decoded
  call $hydra.lib.eithers.map
  local.set $decode_variant
  i32.const 0
  i32.const 1838
  local.get $key
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
  local.get $key
  drop
  local.get $val
  drop
  local.get $ft
  i32.load offset=4
  drop
  local.get $decode_variant
  drop
  i32.const 0
  local.get $ft
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.get $key
  call $hydra.lib.equality.equal
  local.get $fts
  call $hydra.lib.lists.find
  call $hydra.lib.maybes.maybe
  local.set $find_and_decode
  i32.const 0
  i32.const 1478
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
  local.get $k
  drop
  local.get $k
  local.get $obj
  call $hydra.lib.maps.lookup
  drop
  local.get $rt
  drop
  local.get $find_and_decode
  drop
  i32.const 0
  local.get $obj
  call $hydra.lib.maps.keys
  call $hydra.lib.lists.maybe_head
  call $hydra.lib.maybes.maybe
  local.set $decode_single_key
  local.get $obj
  call $hydra.lib.maps.keys
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.equality.equal
  local.get $obj
  drop
  local.get $decode_single_key
  drop
  i32.const 0
  i32.const 0
  i32.const 1478
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
  local.set $process_union
  local.get $value
  call $hydra.json.decode.expect_object
  local.set $obj_result
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
  local.get $obj
  drop
  local.get $process_union
  drop
  i32.const 0
  local.get $obj_result
  call $hydra.lib.eithers.either
  br $end_type
)
  local.get $v
  drop
  local.get $value
  call $hydra.json.decode.expect_object
  local.set $obj_result
  i32.const 17
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
  local.get $obj_result
  call $hydra.lib.eithers.map
  br $end_type
)
  local.get $v
  drop
  local.get $types
  local.get $tname
  local.get $wn
  local.get $value
  call $hydra.json.decode.from_json
  local.set $decoded
  i32.const 20
  local.get $tname
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
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $decoded
  call $hydra.lib.eithers.map
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
  local.get $value
  call $hydra.json.decode.expect_array
  local.set $arr_result
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
  local.get $entry_json
  call $hydra.json.decode.expect_object
  local.set $obj_result
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
  i32.const 1055
  local.get $entry_obj
  call $hydra.lib.maps.lookup
  local.set $key_json
  i32.const 1093
  local.get $entry_obj
  call $hydra.lib.maps.lookup
  local.set $val_json
  i32.const 0
  i32.const 1724
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
  i32.const 0
  i32.const 1780
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
  local.get $key_type
  local.get $kj
  call $hydra.json.decode.from_json
  local.set $decoded_key
  local.get $types
  local.get $tname
  local.get $val_type
  local.get $vj
  call $hydra.json.decode.from_json
  local.set $decoded_val
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
  local.get $k
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
  local.get $decoded_val
  call $hydra.lib.eithers.map
  local.get $decoded_key
  call $hydra.lib.eithers.either
  local.get $val_json
  call $hydra.lib.maybes.maybe
  local.get $key_json
  call $hydra.lib.maybes.maybe
  local.get $obj_result
  call $hydra.lib.eithers.either
  local.set $decode_entry
  local.get $decode_entry
  local.get $arr
  call $hydra.lib.eithers.map_list
  local.set $entries
  i32.const 9
  local.get $es
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
  local.get $entries
  call $hydra.lib.eithers.map
  local.get $arr_result
  call $hydra.lib.eithers.either
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
  local.get $value
  call $hydra.json.decode.expect_object
  local.set $obj_result
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
  i32.const 1045
  local.get $obj
  call $hydra.lib.maps.lookup
  local.set $first_json
  i32.const 1082
  local.get $obj
  call $hydra.lib.maps.lookup
  local.set $second_json
  i32.const 0
  i32.const 1698
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
  i32.const 0
  i32.const 1753
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
  local.get $first_type
  local.get $fj
  call $hydra.json.decode.from_json
  local.set $decoded_first
  local.get $types
  local.get $tname
  local.get $second_type
  local.get $sj
  call $hydra.json.decode.from_json
  local.set $decoded_second
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
  i32.const 11
  local.get $f
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
  local.get $decoded_second
  call $hydra.lib.eithers.map
  local.get $decoded_first
  call $hydra.lib.eithers.either
  local.get $second_json
  call $hydra.lib.maybes.maybe
  local.get $first_json
  call $hydra.lib.maybes.maybe
  local.get $obj_result
  call $hydra.lib.eithers.either
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
  local.get $value
  call $hydra.json.decode.expect_object
  local.set $obj_result
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
  i32.const 1063
  local.get $obj
  call $hydra.lib.maps.lookup
  local.set $left_json
  i32.const 1072
  local.get $obj
  call $hydra.lib.maps.lookup
  local.set $right_json
  i32.const 0
  i32.const 1122
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
  local.get $right_type
  local.get $rj
  call $hydra.json.decode.from_json
  local.set $decoded
  i32.const 3
  i32.const 1
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
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $decoded
  call $hydra.lib.eithers.map
  local.get $right_json
  call $hydra.lib.maybes.maybe
  local.get $types
  local.get $tname
  local.get $left_type
  local.get $lj
  call $hydra.json.decode.from_json
  local.set $decoded
  i32.const 3
  i32.const 0
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
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=4
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
  local.get $decoded
  call $hydra.lib.eithers.map
  local.get $left_json
  call $hydra.lib.maybes.maybe
  local.get $obj_result
  call $hydra.lib.eithers.either
  br $end_type
)
  local.get $v
  drop
  local.get $name
  local.get $types
  call $hydra.lib.maps.lookup
  local.set $looked_up
  i32.const 0
  i32.const 1811
  local.get $name
  drop
  i32.const 0
  drop
  i32.const 0
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
  local.get $types
  local.get $name
  local.get $resolved_type
  local.get $value
  call $hydra.json.decode.from_json
  local.get $looked_up
  call $hydra.lib.maybes.maybe
  br $end_type
)
)
  (func $hydra.json.decode.parse_special_float (param $s i32) (result i32)
  local.get $s
  i32.const 1115
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
  local.get $s
  call $hydra.lib.literals.read_float64
  i32.const 0
  call $hydra.lib.logic.if_else
)
)
