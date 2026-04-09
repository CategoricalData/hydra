(module
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.head" (func $hydra.lib.lists.head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.tail" (func $hydra.lib.lists.tail (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigfloat_to_bigint" (func $hydra.lib.literals.bigfloat_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigfloat_to_float64" (func $hydra.lib.literals.bigfloat_to_float64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_int16" (func $hydra.lib.literals.bigint_to_int16 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_int32" (func $hydra.lib.literals.bigint_to_int32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_int8" (func $hydra.lib.literals.bigint_to_int8 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_uint16" (func $hydra.lib.literals.bigint_to_uint16 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_uint8" (func $hydra.lib.literals.bigint_to_uint8 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.read_bigint" (func $hydra.lib.literals.read_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.read_float32" (func $hydra.lib.literals.read_float32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.read_float64" (func $hydra.lib.literals.read_float64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.read_int64" (func $hydra.lib.literals.read_int64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.read_uint32" (func $hydra.lib.literals.read_uint32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.read_uint64" (func $hydra.lib.literals.read_uint64 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.string_to_binary" (func $hydra.lib.literals.string_to_binary (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.or" (func $hydra.lib.logic.or (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.keys" (func $hydra.lib.maps.keys (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.from_maybe" (func $hydra.lib.maybes.from_maybe (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
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
  br_table $bigfloat $float32 $float64 $float64
)
  (block $end_value (result i32)
  (block $number
  local.get $value
  br_table $number $number
)
  i32.const 1
  local.get $n
  br $end_value
)
  br $end_float_type
)
  local.get $value
  call $hydra.json.decode.expect_string
  local.set $str_result
  i32.const 0
  local.get $err
  local.get $s
  call $hydra.lib.literals.read_float32
  local.set $parsed
  i32.const 0
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "invalid float32: "
  local.get $s
  call $hydra.lib.strings.cat
  i32.const 1
  local.get $v
  local.get $parsed
  call $hydra.lib.maybes.maybe
  local.get $str_result
  call $hydra.lib.eithers.either
  br $end_float_type
)
  (block $end_value (result i32)
  (block $string
  (block $number
  local.get $value
  br_table $number $string $string
)
  i32.const 1
  local.get $n
  call $hydra.lib.literals.bigfloat_to_float64
  br $end_value
)
  i32.const 0
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "invalid float64 sentinel: "
  local.get $s
  call $hydra.lib.strings.cat
  i32.const 1
  local.get $v
  local.get $s
  call $hydra.json.decode.parse_special_float
  call $hydra.lib.maybes.maybe
  br $end_value
)
  br $end_float_type
)
)
  (func $hydra.json.decode.decode_integer (param $it i32) (param $value i32) (result i32)
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
  br_table $bigint $int64 $uint32 $uint64 $int8 $int16 $int32 $uint8 $uint16 $uint16
)
  local.get $value
  call $hydra.json.decode.expect_string
  local.set $str_result
  i32.const 0
  local.get $err
  local.get $s
  call $hydra.lib.literals.read_bigint
  local.set $parsed
  i32.const 0
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "invalid bigint: "
  local.get $s
  call $hydra.lib.strings.cat
  i32.const 1
  local.get $v
  local.get $parsed
  call $hydra.lib.maybes.maybe
  local.get $str_result
  call $hydra.lib.eithers.either
  br $end_integer_type
)
  local.get $value
  call $hydra.json.decode.expect_string
  local.set $str_result
  i32.const 0
  local.get $err
  local.get $s
  call $hydra.lib.literals.read_int64
  local.set $parsed
  i32.const 0
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "invalid int64: "
  local.get $s
  call $hydra.lib.strings.cat
  i32.const 1
  local.get $v
  local.get $parsed
  call $hydra.lib.maybes.maybe
  local.get $str_result
  call $hydra.lib.eithers.either
  br $end_integer_type
)
  local.get $value
  call $hydra.json.decode.expect_string
  local.set $str_result
  i32.const 0
  local.get $err
  local.get $s
  call $hydra.lib.literals.read_uint32
  local.set $parsed
  i32.const 0
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "invalid uint32: "
  local.get $s
  call $hydra.lib.strings.cat
  i32.const 1
  local.get $v
  local.get $parsed
  call $hydra.lib.maybes.maybe
  local.get $str_result
  call $hydra.lib.eithers.either
  br $end_integer_type
)
  local.get $value
  call $hydra.json.decode.expect_string
  local.set $str_result
  i32.const 0
  local.get $err
  local.get $s
  call $hydra.lib.literals.read_uint64
  local.set $parsed
  i32.const 0
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "invalid uint64: "
  local.get $s
  call $hydra.lib.strings.cat
  i32.const 1
  local.get $v
  local.get $parsed
  call $hydra.lib.maybes.maybe
  local.get $str_result
  call $hydra.lib.eithers.either
  br $end_integer_type
)
  local.get $value
  call $hydra.json.decode.expect_number
  local.set $num_result
  local.get $n
  call $hydra.lib.literals.bigfloat_to_bigint
  call $hydra.lib.literals.bigint_to_int8
  local.get $num_result
  call $hydra.lib.eithers.map
  br $end_integer_type
)
  local.get $value
  call $hydra.json.decode.expect_number
  local.set $num_result
  local.get $n
  call $hydra.lib.literals.bigfloat_to_bigint
  call $hydra.lib.literals.bigint_to_int16
  local.get $num_result
  call $hydra.lib.eithers.map
  br $end_integer_type
)
  local.get $value
  call $hydra.json.decode.expect_number
  local.set $num_result
  local.get $n
  call $hydra.lib.literals.bigfloat_to_bigint
  call $hydra.lib.literals.bigint_to_int32
  local.get $num_result
  call $hydra.lib.eithers.map
  br $end_integer_type
)
  local.get $value
  call $hydra.json.decode.expect_number
  local.set $num_result
  local.get $n
  call $hydra.lib.literals.bigfloat_to_bigint
  call $hydra.lib.literals.bigint_to_uint8
  local.get $num_result
  call $hydra.lib.eithers.map
  br $end_integer_type
)
  local.get $value
  call $hydra.json.decode.expect_number
  local.set $num_result
  local.get $n
  call $hydra.lib.literals.bigfloat_to_bigint
  call $hydra.lib.literals.bigint_to_uint16
  local.get $num_result
  call $hydra.lib.eithers.map
  br $end_integer_type
)
)
  (func $hydra.json.decode.decode_literal (param $lt i32) (param $value i32) (result i32)
  (local $b i32)
  (local $ft i32)
  (local $it i32)
  (local $s i32)
  (local $str_result i32)
  (block $end_literal_type (result i32)
  (block $string
  (block $integer
  (block $float
  (block $boolean
  (block $binary
  local.get $lt
  br_table $binary $boolean $float $integer $string $string
)
  local.get $value
  call $hydra.json.decode.expect_string
  local.set $str_result
  local.get $s
  call $hydra.lib.literals.string_to_binary
  local.get $str_result
  call $hydra.lib.eithers.map
  br $end_literal_type
)
  (block $end_value (result i32)
  (block $boolean
  local.get $value
  br_table $boolean $boolean
)
  i32.const 1
  local.get $b
  br $end_value
)
  br $end_literal_type
)
  local.get $ft
  local.get $value
  call $hydra.json.decode.decode_float
  br $end_literal_type
)
  local.get $it
  local.get $value
  call $hydra.json.decode.decode_integer
  br $end_literal_type
)
  local.get $value
  call $hydra.json.decode.expect_string
  local.set $str_result
  local.get $s
  local.get $str_result
  call $hydra.lib.eithers.map
  br $end_literal_type
)
)
  (func $hydra.json.decode.expect_array (param $value i32) (result i32)
  (local $arr i32)
  (block $end_value (result i32)
  (block $array
  local.get $value
  br_table $array $array
)
  i32.const 1
  local.get $arr
  br $end_value
)
)
  (func $hydra.json.decode.expect_number (param $value i32) (result i32)
  (local $n i32)
  (block $end_value (result i32)
  (block $number
  local.get $value
  br_table $number $number
)
  i32.const 1
  local.get $n
  br $end_value
)
)
  (func $hydra.json.decode.expect_object (param $value i32) (result i32)
  (local $obj i32)
  (block $end_value (result i32)
  (block $object
  local.get $value
  br_table $object $object
)
  i32.const 1
  local.get $obj
  br $end_value
)
)
  (func $hydra.json.decode.expect_string (param $value i32) (result i32)
  (local $s i32)
  (block $end_value (result i32)
  (block $string
  local.get $value
  br_table $string $string
)
  i32.const 1
  local.get $s
  br $end_value
)
)
  (func $hydra.json.decode.from_json (param $types i32) (param $tname i32) (param $typ i32) (param $value i32) (result i32)
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
  (local $r i32)
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
  (local $try_field i32)
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
  br_table $literal $list $set $maybe $record $union $unit $wrap $map $pair $either $variable $variable
)
  local.get $lt
  local.get $value
  call $hydra.json.decode.decode_literal
  br $end_type
)
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
  local.get $decode_elem
  local.get $arr
  call $hydra.lib.eithers.map_list
  local.set $decoded
  local.get $ts
  local.get $decoded
  call $hydra.lib.eithers.map
  local.get $arr_result
  call $hydra.lib.eithers.either
  br $end_type
)
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
  local.get $decode_elem
  local.get $arr
  call $hydra.lib.eithers.map_list
  local.set $decoded
  local.get $elems
  call $hydra.lib.sets.from_list
  local.get $decoded
  call $hydra.lib.eithers.map
  local.get $arr_result
  call $hydra.lib.eithers.either
  br $end_type
)
  local.get $inner_type
  call $hydra.strip.deannotate_type
  local.set $inner_stripped
  (block $end_type (result i32)
  (block $maybe
  local.get $inner_stripped
  br_table $maybe $maybe
)
  i32.const 1
  br $end_type
)
  local.set $is_nested_maybe
  local.get $is_nested_maybe
  local.get $v
  local.get $types
  local.get $tname
  local.get $inner_type
  local.get $arr
  call $hydra.lib.lists.head
  call $hydra.json.decode.from_json
  call $hydra.lib.eithers.map
  local.set $decode_just
  local.get $arr
  call $hydra.lib.lists.length
  local.set $len
  local.get $len
  i32.const 0
  call $hydra.lib.equality.equal
  i32.const 1
  i32.const 0
  local.get $len
  i32.const 1
  call $hydra.lib.equality.equal
  local.get $arr
  local.get $decode_just
  i32.const 0
  i32.const 0 ;; string: "expected single-element array for Just"
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  local.set $decode_maybe_array
  (block $end_value (result i32)
  (block $array
  (block $null
  local.get $value
  br_table $null $array $array
)
  i32.const 1
  i32.const 0
  br $end_value
)
  local.get $arr
  local.get $decode_maybe_array
  br $end_value
)
  (block $end_value (result i32)
  (block $null
  local.get $value
  br_table $null $null
)
  i32.const 1
  i32.const 0
  br $end_value
)
  call $hydra.lib.logic.if_else
  br $end_type
)
  local.get $value
  call $hydra.json.decode.expect_object
  local.set $obj_result
  i32.const 0
  local.get $err
  local.get $ft
  ;; project field: name
  local.set $fname
  local.get $ft
  ;; project field: type
  local.set $ftype
  nop
  local.get $obj
  call $hydra.lib.maps.lookup
  local.set $mval
  i32.const 0
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
  local.get $decoded
  call $hydra.lib.eithers.map
  local.set $decode_field
  local.get $decode_field
  local.get $rt
  call $hydra.lib.eithers.map_list
  local.set $decoded_fields
  local.get $tname
  local.get $fs
  local.get $decoded_fields
  call $hydra.lib.eithers.map
  local.get $obj_result
  call $hydra.lib.eithers.either
  br $end_type
)
  i32.const 0
  local.get $val
  call $hydra.lib.maybes.from_maybe
  local.set $json_val
  local.get $types
  local.get $tname
  local.get $ftype
  local.get $json_val
  call $hydra.json.decode.from_json
  local.set $decoded
  local.get $tname
  local.get $key
  local.get $v
  local.get $decoded
  call $hydra.lib.eithers.map
  local.set $decode_variant
  nop
  local.get $key
  call $hydra.lib.equality.equal
  local.get $key
  local.get $val
  local.get $ft
  ;; project field: type
  local.get $decode_variant
  i32.const 0
  call $hydra.lib.logic.if_else
  local.set $try_field
  local.get $fts
  call $hydra.lib.lists.null
  i32.const 0
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "unknown variant: "
  local.get $key
  call $hydra.lib.strings.cat
  local.get $key
  local.get $val
  local.get $fts
  call $hydra.lib.lists.tail
  local.get $find_and_decode
  local.get $r
  local.get $key
  local.get $val
  local.get $fts
  call $hydra.lib.lists.head
  local.get $try_field
  call $hydra.lib.maybes.maybe
  call $hydra.lib.logic.if_else
  local.set $find_and_decode
  local.get $obj
  call $hydra.lib.maps.keys
  call $hydra.lib.lists.head
  local.get $obj
  call $hydra.lib.maps.keys
  call $hydra.lib.lists.head
  local.get $obj
  call $hydra.lib.maps.lookup
  local.get $rt
  local.get $find_and_decode
  local.set $decode_single_key
  local.get $obj
  call $hydra.lib.maps.keys
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.equality.equal
  local.get $obj
  local.get $decode_single_key
  i32.const 0
  i32.const 0 ;; string: "expected single-key object for union"
  call $hydra.lib.logic.if_else
  local.set $process_union
  local.get $value
  call $hydra.json.decode.expect_object
  local.set $obj_result
  i32.const 0
  local.get $err
  local.get $obj
  local.get $process_union
  local.get $obj_result
  call $hydra.lib.eithers.either
  br $end_type
)
  local.get $value
  call $hydra.json.decode.expect_object
  local.set $obj_result
  i32.const 0
  local.get $obj_result
  call $hydra.lib.eithers.map
  br $end_type
)
  local.get $types
  local.get $tname
  local.get $wn
  local.get $value
  call $hydra.json.decode.from_json
  local.set $decoded
  local.get $tname
  local.get $v
  local.get $decoded
  call $hydra.lib.eithers.map
  br $end_type
)
  local.get $mt
  ;; project field: keys
  local.set $key_type
  local.get $mt
  ;; project field: values
  local.set $val_type
  local.get $value
  call $hydra.json.decode.expect_array
  local.set $arr_result
  i32.const 0
  local.get $err
  local.get $entry_json
  call $hydra.json.decode.expect_object
  local.set $obj_result
  i32.const 0
  local.get $err
  i32.const 0 ;; string: "@key"
  local.get $entry_obj
  call $hydra.lib.maps.lookup
  local.set $key_json
  i32.const 0 ;; string: "@value"
  local.get $entry_obj
  call $hydra.lib.maps.lookup
  local.set $val_json
  i32.const 0
  i32.const 0 ;; string: "missing @key in map entry"
  i32.const 0
  i32.const 0 ;; string: "missing @value in map entry"
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
  local.get $k
  local.get $v
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
  local.get $es
  call $hydra.lib.maps.from_list
  local.get $entries
  call $hydra.lib.eithers.map
  local.get $arr_result
  call $hydra.lib.eithers.either
  br $end_type
)
  local.get $pt
  ;; project field: first
  local.set $first_type
  local.get $pt
  ;; project field: second
  local.set $second_type
  local.get $value
  call $hydra.json.decode.expect_object
  local.set $obj_result
  i32.const 0
  local.get $err
  i32.const 0 ;; string: "@first"
  local.get $obj
  call $hydra.lib.maps.lookup
  local.set $first_json
  i32.const 0 ;; string: "@second"
  local.get $obj
  call $hydra.lib.maps.lookup
  local.set $second_json
  i32.const 0
  i32.const 0 ;; string: "missing @first in pair"
  i32.const 0
  i32.const 0 ;; string: "missing @second in pair"
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
  local.get $f
  local.get $s
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
  local.get $et
  ;; project field: left
  local.set $left_type
  local.get $et
  ;; project field: right
  local.set $right_type
  local.get $value
  call $hydra.json.decode.expect_object
  local.set $obj_result
  i32.const 0
  local.get $err
  i32.const 0 ;; string: "@left"
  local.get $obj
  call $hydra.lib.maps.lookup
  local.set $left_json
  i32.const 0 ;; string: "@right"
  local.get $obj
  call $hydra.lib.maps.lookup
  local.set $right_json
  i32.const 0
  i32.const 0 ;; string: "expected @left or @right in Either"
  local.get $types
  local.get $tname
  local.get $right_type
  local.get $rj
  call $hydra.json.decode.from_json
  local.set $decoded
  i32.const 1
  local.get $v
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
  i32.const 0
  local.get $v
  local.get $decoded
  call $hydra.lib.eithers.map
  local.get $left_json
  call $hydra.lib.maybes.maybe
  local.get $obj_result
  call $hydra.lib.eithers.either
  br $end_type
)
  local.get $name
  local.get $types
  call $hydra.lib.maps.lookup
  local.set $looked_up
  i32.const 0
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "unknown type variable: "
  nop
  call $hydra.lib.strings.cat
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
  i32.const 0 ;; string: "NaN"
  call $hydra.lib.equality.equal
  local.get $s
  i32.const 0 ;; string: "Infinity"
  call $hydra.lib.equality.equal
  local.get $s
  i32.const 0 ;; string: "-Infinity"
  call $hydra.lib.equality.equal
  call $hydra.lib.logic.or
  call $hydra.lib.logic.or
  local.get $s
  call $hydra.lib.literals.read_float64
  i32.const 0
  call $hydra.lib.logic.if_else
)
)
