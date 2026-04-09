(module
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.head" (func $hydra.lib.lists.head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.tail" (func $hydra.lib.lists.tail (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.zip" (func $hydra.lib.lists.zip (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_bigfloat" (func $hydra.lib.literals.bigint_to_bigfloat (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.binary_to_string" (func $hydra.lib.literals.binary_to_string (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.float64_to_bigfloat" (func $hydra.lib.literals.float64_to_bigfloat (param i32) (result i32) ) )
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
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.or" (func $hydra.lib.logic.or (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.empty" (func $hydra.lib.maps.empty (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.cat" (func $hydra.lib.maybes.cat (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_term" (func $hydra.strip.deannotate_term (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.json.encode.encode_float" (func $hydra.json.encode.encode_float) )
  (export "hydra.json.encode.encode_integer" (func $hydra.json.encode.encode_integer) )
  (export "hydra.json.encode.encode_literal" (func $hydra.json.encode.encode_literal) )
  (export "hydra.json.encode.is_special_float_string" (func $hydra.json.encode.is_special_float_string) )
  (export "hydra.json.encode.to_json" (func $hydra.json.encode.to_json) )
  (export "hydra.json.encode.to_json_untyped" (func $hydra.json.encode.to_json_untyped) )
  (func $hydra.json.encode.encode_float (param $fv i32) (result i32)
  (local $bf i32)
  (local $f i32)
  (local $s i32)
  (block $end_float_value (result i32)
  (block $float64
  (block $float32
  (block $bigfloat
  local.get $fv
  br_table $bigfloat $float32 $float64 $float64
)
  local.get $bf
  call $hydra.lib.literals.show_bigfloat
  local.set $s
  local.get $s
  call $hydra.json.encode.is_special_float_string
  i32.const 1
  local.get $s
  i32.const 1
  local.get $bf
  call $hydra.lib.logic.if_else
  br $end_float_value
)
  i32.const 1
  local.get $f
  call $hydra.lib.literals.show_float32
  br $end_float_value
)
  local.get $f
  call $hydra.lib.literals.show_float64
  local.set $s
  local.get $s
  call $hydra.json.encode.is_special_float_string
  i32.const 1
  local.get $s
  i32.const 1
  local.get $f
  call $hydra.lib.literals.float64_to_bigfloat
  call $hydra.lib.logic.if_else
  br $end_float_value
)
)
  (func $hydra.json.encode.encode_integer (param $iv i32) (result i32)
  (local $bi i32)
  (local $i i32)
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
  br_table $bigint $int64 $uint32 $uint64 $int8 $int16 $int32 $uint8 $uint16 $uint16
)
  i32.const 1
  local.get $bi
  call $hydra.lib.literals.show_bigint
  br $end_integer_value
)
  i32.const 1
  local.get $i
  call $hydra.lib.literals.show_int64
  br $end_integer_value
)
  i32.const 1
  local.get $i
  call $hydra.lib.literals.show_uint32
  br $end_integer_value
)
  i32.const 1
  local.get $i
  call $hydra.lib.literals.show_uint64
  br $end_integer_value
)
  i32.const 1
  local.get $i
  call $hydra.lib.literals.int8_to_bigint
  call $hydra.lib.literals.bigint_to_bigfloat
  br $end_integer_value
)
  i32.const 1
  local.get $i
  call $hydra.lib.literals.int16_to_bigint
  call $hydra.lib.literals.bigint_to_bigfloat
  br $end_integer_value
)
  i32.const 1
  local.get $i
  call $hydra.lib.literals.int32_to_bigint
  call $hydra.lib.literals.bigint_to_bigfloat
  br $end_integer_value
)
  i32.const 1
  local.get $i
  call $hydra.lib.literals.uint8_to_bigint
  call $hydra.lib.literals.bigint_to_bigfloat
  br $end_integer_value
)
  i32.const 1
  local.get $i
  call $hydra.lib.literals.uint16_to_bigint
  call $hydra.lib.literals.bigint_to_bigfloat
  br $end_integer_value
)
)
  (func $hydra.json.encode.encode_literal (param $lit i32) (result i32)
  (local $b i32)
  (local $f i32)
  (local $i i32)
  (local $s i32)
  (block $end_literal (result i32)
  (block $string
  (block $integer
  (block $float
  (block $boolean
  (block $binary
  local.get $lit
  br_table $binary $boolean $float $integer $string $string
)
  i32.const 1
  local.get $b
  call $hydra.lib.literals.binary_to_string
  br $end_literal
)
  i32.const 1
  local.get $b
  br $end_literal
)
  local.get $f
  call $hydra.json.encode.encode_float
  br $end_literal
)
  local.get $i
  call $hydra.json.encode.encode_integer
  br $end_literal
)
  i32.const 1
  local.get $s
  br $end_literal
)
)
  (func $hydra.json.encode.is_special_float_string (param $s i32) (result i32)
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
)
  (func $hydra.json.encode.to_json (param $types i32) (param $tname i32) (param $typ i32) (param $term i32) (result i32)
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
  (local $find_field_type i32)
  (local $first i32)
  (local $first_type i32)
  (local $fname i32)
  (local $ft i32)
  (local $fterm i32)
  (local $ftf i32)
  (local $fts i32)
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
  br_table $literal $list $set $maybe $record $union $unit $wrap $map $pair $either $variable $variable
)
  (block $end_term (result i32)
  (block $literal
  local.get $stripped_term
  br_table $literal $literal
)
  local.get $lit
  call $hydra.json.encode.encode_literal
  br $end_term
)
  br $end_type
)
  (block $end_term (result i32)
  (block $list
  local.get $stripped_term
  br_table $list $list
)
  local.get $types
  local.get $tname
  local.get $elem_type
  local.get $t
  call $hydra.json.encode.to_json
  local.get $terms
  call $hydra.lib.eithers.map_list
  local.set $results
  local.get $vs
  local.get $results
  call $hydra.lib.eithers.map
  br $end_term
)
  br $end_type
)
  (block $end_term (result i32)
  (block $set
  local.get $stripped_term
  br_table $set $set
)
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
  local.get $vs
  local.get $results
  call $hydra.lib.eithers.map
  br $end_term
)
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
  (block $end_term (result i32)
  (block $maybe
  local.get $stripped_term
  br_table $maybe $maybe
)
  i32.const 1
  i32.const 0
  local.get $types
  local.get $tname
  local.get $inner_type
  local.get $v
  call $hydra.json.encode.to_json
  local.set $encoded
  local.get $is_nested_maybe
  i32.const 1
  ;; list elements follow
  local.get $ev
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
  (block $end_term (result i32)
  (block $record
  local.get $stripped_term
  br_table $record $record
)
  (block $end_type (result i32)
  (block $maybe
  local.get $ftype
  call $hydra.strip.deannotate_type
  br_table $maybe $maybe
)
  (block $end_type (result i32)
  (block $maybe
  local.get $inner_t
  call $hydra.strip.deannotate_type
  br_table $maybe $maybe
)
  i32.const 0
  br $end_type
)
  br $end_type
)
  local.set $is_simple_maybe
  nop
  local.set $fname
  local.get $f
  ;; project field: term
  local.set $fterm
  local.get $ft
  ;; project field: type
  local.set $ftype
  local.get $ftype
  local.get $is_simple_maybe
  (block $end_term (result i32)
  (block $maybe
  local.get $fterm
  call $hydra.strip.deannotate_term
  br_table $maybe $maybe
)
  i32.const 1
  i32.const 0
  (block $end_type (result i32)
  (block $maybe
  local.get $ftype
  call $hydra.strip.deannotate_type
  br_table $maybe $maybe
)
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
  local.get $encoded
  call $hydra.lib.eithers.map
  call $hydra.lib.logic.if_else
  local.set $encode_field_with_type
  local.get $rt
  local.set $field_types
  local.get $r
  ;; project field: fields
  local.set $fields
  local.get $ftf
  call $hydra.lib.pairs.first
  local.get $ftf
  call $hydra.lib.pairs.second
  local.get $encode_field_with_type
  local.get $field_types
  local.get $fields
  call $hydra.lib.lists.zip
  call $hydra.lib.eithers.map_list
  local.set $encoded_pairs
  local.get $pairs
  call $hydra.lib.maybes.cat
  call $hydra.lib.maps.from_list
  local.get $encoded_pairs
  call $hydra.lib.eithers.map
  br $end_term
)
  br $end_type
)
  (block $end_term (result i32)
  (block $union
  local.get $stripped_term
  br_table $union $union
)
  local.get $inj
  ;; project field: field
  local.set $field
  nop
  local.set $fname
  local.get $field
  ;; project field: term
  local.set $fterm
  local.get $fts
  call $hydra.lib.lists.null
  i32.const 0
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "unknown variant: "
  local.get $fname
  call $hydra.lib.strings.cat
  nop
  local.get $fname
  call $hydra.lib.equality.equal
  i32.const 1
  local.get $fts
  call $hydra.lib.lists.head
  ;; project field: type
  local.get $fts
  call $hydra.lib.lists.tail
  local.get $find_field_type
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  local.set $find_field_type
  local.get $rt
  local.get $find_field_type
  local.set $ftype_result
  i32.const 0
  local.get $err
  local.get $types
  local.get $tname
  local.get $ftype
  local.get $fterm
  call $hydra.json.encode.to_json
  local.set $encoded_union
  i32.const 1
  ;; list elements follow
  local.get $fname
  local.get $v
  call $hydra.lib.maps.from_list
  local.get $encoded_union
  call $hydra.lib.eithers.map
  local.get $ftype_result
  call $hydra.lib.eithers.either
  br $end_term
)
  br $end_type
)
  i32.const 1
  call $hydra.lib.maps.empty
  br $end_type
)
  (block $end_term (result i32)
  (block $wrap
  local.get $stripped_term
  br_table $wrap $wrap
)
  local.get $types
  local.get $tname
  local.get $wn
  local.get $wt
  ;; project field: body
  call $hydra.json.encode.to_json
  br $end_term
)
  br $end_type
)
  local.get $mt
  ;; project field: keys
  local.set $key_type
  local.get $mt
  ;; project field: values
  local.set $val_type
  (block $end_term (result i32)
  (block $map
  local.get $stripped_term
  br_table $map $map
)
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
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "@key"
  local.get $ek
  i32.const 0 ;; string: "@value"
  local.get $ev
  call $hydra.lib.maps.from_list
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
  local.get $es
  local.get $entries
  call $hydra.lib.eithers.map
  br $end_term
)
  br $end_type
)
  local.get $pt
  ;; project field: first
  local.set $first_type
  local.get $pt
  ;; project field: second
  local.set $second_type
  (block $end_term (result i32)
  (block $pair
  local.get $stripped_term
  br_table $pair $pair
)
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
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "@first"
  local.get $ef
  i32.const 0 ;; string: "@second"
  local.get $es
  call $hydra.lib.maps.from_list
  local.get $encoded_second
  call $hydra.lib.eithers.map
  local.get $encoded_first
  call $hydra.lib.eithers.either
  br $end_term
)
  br $end_type
)
  local.get $et
  ;; project field: left
  local.set $left_type
  local.get $et
  ;; project field: right
  local.set $right_type
  (block $end_term (result i32)
  (block $either
  local.get $stripped_term
  br_table $either $either
)
  local.get $types
  local.get $tname
  local.get $left_type
  local.get $l
  call $hydra.json.encode.to_json
  local.set $encoded_l
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "@left"
  local.get $v
  call $hydra.lib.maps.from_list
  local.get $encoded_l
  call $hydra.lib.eithers.map
  local.get $types
  local.get $tname
  local.get $right_type
  local.get $r
  call $hydra.json.encode.to_json
  local.set $encoded_r
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "@right"
  local.get $v
  call $hydra.lib.maps.from_list
  local.get $encoded_r
  call $hydra.lib.eithers.map
  local.get $e
  call $hydra.lib.eithers.either
  br $end_term
)
  br $end_type
)
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
  (block $union
  (block $record
  (block $maybe
  (block $set
  (block $list
  (block $literal
  local.get $stripped
  br_table $literal $list $set $maybe $record $union $unit $wrap $map $pair $either $either
)
  local.get $lit
  call $hydra.json.encode.encode_literal
  br $end_term
)
  local.get $t
  call $hydra.json.encode.to_json_untyped
  local.get $terms
  call $hydra.lib.eithers.map_list
  local.set $results
  local.get $vs
  local.get $results
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $vals
  call $hydra.lib.sets.to_list
  local.set $terms
  local.get $t
  call $hydra.json.encode.to_json_untyped
  local.get $terms
  call $hydra.lib.eithers.map_list
  local.set $results
  local.get $vs
  local.get $results
  call $hydra.lib.eithers.map
  br $end_term
)
  i32.const 1
  i32.const 0
  local.get $v
  call $hydra.json.encode.to_json_untyped
  local.set $encoded_maybe
  i32.const 1
  ;; list elements follow
  local.get $encoded
  local.get $encoded_maybe
  call $hydra.lib.eithers.map
  local.get $opt
  call $hydra.lib.maybes.maybe
  br $end_term
)
  nop
  local.set $fname
  local.get $f
  ;; project field: term
  local.set $fterm
  local.get $fterm
  call $hydra.json.encode.to_json_untyped
  local.set $encoded_field
  local.get $fname
  local.get $v
  local.get $encoded_field
  call $hydra.lib.eithers.map
  local.set $encode_field
  local.get $r
  ;; project field: fields
  local.set $fields
  local.get $encode_field
  local.get $fields
  call $hydra.lib.eithers.map_list
  local.set $encoded_fields
  local.get $fs
  call $hydra.lib.maps.from_list
  local.get $encoded_fields
  call $hydra.lib.eithers.map
  br $end_term
)
  local.get $inj
  ;; project field: field
  local.set $field
  nop
  local.set $fname
  local.get $field
  ;; project field: term
  local.set $fterm
  local.get $fterm
  call $hydra.json.encode.to_json_untyped
  local.set $encoded_union
  i32.const 1
  ;; list elements follow
  local.get $fname
  local.get $v
  call $hydra.lib.maps.from_list
  local.get $encoded_union
  call $hydra.lib.eithers.map
  br $end_term
)
  i32.const 1
  call $hydra.lib.maps.empty
  br $end_term
)
  local.get $wt
  ;; project field: body
  call $hydra.json.encode.to_json_untyped
  br $end_term
)
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
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "@key"
  local.get $ek
  i32.const 0 ;; string: "@value"
  local.get $ev
  call $hydra.lib.maps.from_list
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
  local.get $es
  local.get $entries
  call $hydra.lib.eithers.map
  br $end_term
)
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
  i32.const 2
  ;; list elements follow
  i32.const 0 ;; string: "@first"
  local.get $ef
  i32.const 0 ;; string: "@second"
  local.get $es
  call $hydra.lib.maps.from_list
  local.get $encoded_second
  call $hydra.lib.eithers.map
  local.get $encoded_first
  call $hydra.lib.eithers.either
  br $end_term
)
  local.get $l
  call $hydra.json.encode.to_json_untyped
  local.set $encoded_l
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "@left"
  local.get $v
  call $hydra.lib.maps.from_list
  local.get $encoded_l
  call $hydra.lib.eithers.map
  local.get $r
  call $hydra.json.encode.to_json_untyped
  local.set $encoded_r
  i32.const 1
  ;; list elements follow
  i32.const 0 ;; string: "@right"
  local.get $v
  call $hydra.lib.maps.from_list
  local.get $encoded_r
  call $hydra.lib.eithers.map
  local.get $e
  call $hydra.lib.eithers.either
  br $end_term
)
)
)
