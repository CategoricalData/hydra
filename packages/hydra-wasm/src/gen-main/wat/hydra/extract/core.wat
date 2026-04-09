(module
  (import "hydra.lexical" "hydra.lexical.strip_and_dereference_term" (func $hydra.lexical.strip_and_dereference_term (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.strip_and_dereference_term_either" (func $hydra.lexical.strip_and_dereference_term_either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_maybe" (func $hydra.lib.eithers.map_maybe (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_set" (func $hydra.lib.eithers.map_set (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.filter" (func $hydra.lib.lists.filter (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.head" (func $hydra.lib.lists.head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_string" (func $hydra.lib.literals.show_string (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.pure" (func $hydra.lib.maybes.pure (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.from_list" (func $hydra.lib.sets.from_list (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.term" (func $hydra.show.core.term (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (memory $memory 1 )
  (export "memory" (memory $memory) )
  (export "hydra.extract.core.bigfloat" (func $hydra.extract.core.bigfloat) )
  (export "hydra.extract.core.bigfloat_value" (func $hydra.extract.core.bigfloat_value) )
  (export "hydra.extract.core.bigint" (func $hydra.extract.core.bigint) )
  (export "hydra.extract.core.bigint_value" (func $hydra.extract.core.bigint_value) )
  (export "hydra.extract.core.binary" (func $hydra.extract.core.binary) )
  (export "hydra.extract.core.binary_literal" (func $hydra.extract.core.binary_literal) )
  (export "hydra.extract.core.boolean" (func $hydra.extract.core.boolean) )
  (export "hydra.extract.core.boolean_literal" (func $hydra.extract.core.boolean_literal) )
  (export "hydra.extract.core.case_field" (func $hydra.extract.core.case_field) )
  (export "hydra.extract.core.cases" (func $hydra.extract.core.cases) )
  (export "hydra.extract.core.decode_either" (func $hydra.extract.core.decode_either) )
  (export "hydra.extract.core.decode_list" (func $hydra.extract.core.decode_list) )
  (export "hydra.extract.core.decode_map" (func $hydra.extract.core.decode_map) )
  (export "hydra.extract.core.decode_maybe" (func $hydra.extract.core.decode_maybe) )
  (export "hydra.extract.core.decode_pair" (func $hydra.extract.core.decode_pair) )
  (export "hydra.extract.core.decode_set" (func $hydra.extract.core.decode_set) )
  (export "hydra.extract.core.decode_unit" (func $hydra.extract.core.decode_unit) )
  (export "hydra.extract.core.decode_wrapped" (func $hydra.extract.core.decode_wrapped) )
  (export "hydra.extract.core.either_term" (func $hydra.extract.core.either_term) )
  (export "hydra.extract.core.either_type" (func $hydra.extract.core.either_type) )
  (export "hydra.extract.core.field" (func $hydra.extract.core.field) )
  (export "hydra.extract.core.float32" (func $hydra.extract.core.float32) )
  (export "hydra.extract.core.float32_value" (func $hydra.extract.core.float32_value) )
  (export "hydra.extract.core.float64" (func $hydra.extract.core.float64) )
  (export "hydra.extract.core.float64_value" (func $hydra.extract.core.float64_value) )
  (export "hydra.extract.core.float_literal" (func $hydra.extract.core.float_literal) )
  (export "hydra.extract.core.float_value" (func $hydra.extract.core.float_value) )
  (export "hydra.extract.core.function_type" (func $hydra.extract.core.function_type) )
  (export "hydra.extract.core.injection" (func $hydra.extract.core.injection) )
  (export "hydra.extract.core.int16" (func $hydra.extract.core.int16) )
  (export "hydra.extract.core.int16_value" (func $hydra.extract.core.int16_value) )
  (export "hydra.extract.core.int32" (func $hydra.extract.core.int32) )
  (export "hydra.extract.core.int32_value" (func $hydra.extract.core.int32_value) )
  (export "hydra.extract.core.int64" (func $hydra.extract.core.int64) )
  (export "hydra.extract.core.int64_value" (func $hydra.extract.core.int64_value) )
  (export "hydra.extract.core.int8" (func $hydra.extract.core.int8) )
  (export "hydra.extract.core.int8_value" (func $hydra.extract.core.int8_value) )
  (export "hydra.extract.core.integer_literal" (func $hydra.extract.core.integer_literal) )
  (export "hydra.extract.core.integer_value" (func $hydra.extract.core.integer_value) )
  (export "hydra.extract.core.lambda" (func $hydra.extract.core.lambda) )
  (export "hydra.extract.core.lambda_body" (func $hydra.extract.core.lambda_body) )
  (export "hydra.extract.core.let" (func $hydra.extract.core.let) )
  (export "hydra.extract.core.let_binding" (func $hydra.extract.core.let_binding) )
  (export "hydra.extract.core.list" (func $hydra.extract.core.list) )
  (export "hydra.extract.core.list_head" (func $hydra.extract.core.list_head) )
  (export "hydra.extract.core.list_of" (func $hydra.extract.core.list_of) )
  (export "hydra.extract.core.list_type" (func $hydra.extract.core.list_type) )
  (export "hydra.extract.core.literal" (func $hydra.extract.core.literal) )
  (export "hydra.extract.core.map" (func $hydra.extract.core.map) )
  (export "hydra.extract.core.map_type" (func $hydra.extract.core.map_type) )
  (export "hydra.extract.core.maybe_term" (func $hydra.extract.core.maybe_term) )
  (export "hydra.extract.core.maybe_type" (func $hydra.extract.core.maybe_type) )
  (export "hydra.extract.core.n_args" (func $hydra.extract.core.n_args) )
  (export "hydra.extract.core.pair" (func $hydra.extract.core.pair) )
  (export "hydra.extract.core.record" (func $hydra.extract.core.record) )
  (export "hydra.extract.core.record_type" (func $hydra.extract.core.record_type) )
  (export "hydra.extract.core.require_field" (func $hydra.extract.core.require_field) )
  (export "hydra.extract.core.set" (func $hydra.extract.core.set) )
  (export "hydra.extract.core.set_of" (func $hydra.extract.core.set_of) )
  (export "hydra.extract.core.set_type" (func $hydra.extract.core.set_type) )
  (export "hydra.extract.core.string" (func $hydra.extract.core.string) )
  (export "hydra.extract.core.string_literal" (func $hydra.extract.core.string_literal) )
  (export "hydra.extract.core.term_record" (func $hydra.extract.core.term_record) )
  (export "hydra.extract.core.to_field_map" (func $hydra.extract.core.to_field_map) )
  (export "hydra.extract.core.uint16" (func $hydra.extract.core.uint16) )
  (export "hydra.extract.core.uint16_value" (func $hydra.extract.core.uint16_value) )
  (export "hydra.extract.core.uint32" (func $hydra.extract.core.uint32) )
  (export "hydra.extract.core.uint32_value" (func $hydra.extract.core.uint32_value) )
  (export "hydra.extract.core.uint64" (func $hydra.extract.core.uint64) )
  (export "hydra.extract.core.uint64_value" (func $hydra.extract.core.uint64_value) )
  (export "hydra.extract.core.uint8" (func $hydra.extract.core.uint8) )
  (export "hydra.extract.core.uint8_value" (func $hydra.extract.core.uint8_value) )
  (export "hydra.extract.core.union_type" (func $hydra.extract.core.union_type) )
  (export "hydra.extract.core.unit" (func $hydra.extract.core.unit) )
  (export "hydra.extract.core.unit_variant" (func $hydra.extract.core.unit_variant) )
  (export "hydra.extract.core.wrap" (func $hydra.extract.core.wrap) )
  (export "hydra.extract.core.wrapped_type" (func $hydra.extract.core.wrapped_type) )
  (func $hydra.extract.core.bigfloat (param $cx i32) (param $graph i32) (param $t i32) (result i32)
  (local $f i32)
  (local $l i32)
  local.get $cx
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $cx
  local.get $l
  call $hydra.extract.core.float_literal
  local.get $cx
  local.get $f
  call $hydra.extract.core.bigfloat_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.bigfloat_value (param $cx i32) (param $v i32) (result i32)
  (local $f i32)
  (block $end_float_value (result i32)
  (block $bigfloat
  local.get $v
  br_table $bigfloat $bigfloat
)
  i32.const 1
  local.get $f
  br $end_float_value
)
)
  (func $hydra.extract.core.bigint (param $cx i32) (param $graph i32) (param $t i32) (result i32)
  (local $i i32)
  (local $l i32)
  local.get $cx
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $cx
  local.get $l
  call $hydra.extract.core.integer_literal
  local.get $cx
  local.get $i
  call $hydra.extract.core.bigint_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.bigint_value (param $cx i32) (param $v i32) (result i32)
  (local $i i32)
  (block $end_integer_value (result i32)
  (block $bigint
  local.get $v
  br_table $bigint $bigint
)
  i32.const 1
  local.get $i
  br $end_integer_value
)
)
  (func $hydra.extract.core.binary (param $cx i32) (param $graph i32) (param $t i32) (result i32)
  (local $l i32)
  local.get $cx
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $cx
  local.get $l
  call $hydra.extract.core.binary_literal
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.binary_literal (param $cx i32) (param $v i32) (result i32)
  (local $b i32)
  (block $end_literal (result i32)
  (block $binary
  local.get $v
  br_table $binary $binary
)
  i32.const 1
  local.get $b
  br $end_literal
)
)
  (func $hydra.extract.core.boolean (param $cx i32) (param $graph i32) (param $t i32) (result i32)
  (local $l i32)
  local.get $cx
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $cx
  local.get $l
  call $hydra.extract.core.boolean_literal
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.boolean_literal (param $cx i32) (param $v i32) (result i32)
  (local $b i32)
  (block $end_literal (result i32)
  (block $boolean
  local.get $v
  br_table $boolean $boolean
)
  i32.const 1
  local.get $b
  br $end_literal
)
)
  (func $hydra.extract.core.case_field (param $cx i32) (param $name i32) (param $n i32) (param $graph i32) (param $term i32) (result i32)
  (local $cs i32)
  (local $field_name i32)
  (local $matching i32)
  local.get $n
  local.set $field_name
  local.get $cx
  local.get $name
  local.get $graph
  local.get $term
  call $hydra.extract.core.cases
  nop
  nop
  call $hydra.lib.equality.equal
  local.get $cs
  ;; project field: cases
  call $hydra.lib.lists.filter
  local.set $matching
  local.get $matching
  call $hydra.lib.lists.null
  i32.const 0
  i32.const 0 ;; string: "not enough cases"
  local.get $cx
  i32.const 1
  local.get $matching
  call $hydra.lib.lists.head
  call $hydra.lib.logic.if_else
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.cases (param $cx i32) (param $name i32) (param $graph i32) (param $term0 i32) (result i32)
  (local $cs i32)
  (local $elimination i32)
  (local $function i32)
  (local $term i32)
  local.get $cx
  local.get $graph
  local.get $term0
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $function
  local.get $term
  br_table $function $function
)
  (block $end_function (result i32)
  (block $elimination
  local.get $function
  br_table $elimination $elimination
)
  (block $end_elimination (result i32)
  (block $union
  local.get $elimination
  br_table $union $union
)
  nop
  nop
  call $hydra.lib.equality.equal
  i32.const 1
  local.get $cs
  i32.const 0
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "case statement for type "
  nop
  call $hydra.lib.strings.cat2
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: " but found "
  call $hydra.lib.strings.cat2
  local.get $term
  call $hydra.show.core.term
  call $hydra.lib.strings.cat2
  local.get $cx
  call $hydra.lib.logic.if_else
  br $end_elimination
)
  br $end_function
)
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.decode_either (param $left_decoder i32) (param $right_decoder i32) (param $g i32) (param $term i32) (result i32)
  (local $e i32)
  (local $lv i32)
  (local $rv i32)
  (local $stripped i32)
  (local $x i32)
  local.get $x
  local.get $x
  local.get $g
  local.get $term
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.bimap
  (block $end_term (result i32)
  (block $either
  local.get $stripped
  br_table $either $either
)
  i32.const 0
  local.get $x
  local.get $g
  local.get $lv
  local.get $left_decoder
  call $hydra.lib.eithers.map
  i32.const 1
  local.get $x
  local.get $g
  local.get $rv
  local.get $right_decoder
  call $hydra.lib.eithers.map
  local.get $e
  call $hydra.lib.eithers.either
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.decode_list (param $elem_decoder i32) (param $g i32) (param $term i32) (result i32)
  (local $els i32)
  (local $stripped i32)
  (local $x i32)
  local.get $x
  local.get $x
  local.get $g
  local.get $term
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.bimap
  (block $end_term (result i32)
  (block $list
  local.get $stripped
  br_table $list $list
)
  local.get $g
  local.get $elem_decoder
  local.get $els
  call $hydra.lib.eithers.map_list
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.decode_map (param $key_decoder i32) (param $val_decoder i32) (param $g i32) (param $term i32) (result i32)
  (local $k i32)
  (local $kv i32)
  (local $m i32)
  (local $stripped i32)
  (local $v i32)
  (local $x i32)
  local.get $x
  local.get $x
  local.get $g
  local.get $term
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.bimap
  (block $end_term (result i32)
  (block $map
  local.get $stripped
  br_table $map $map
)
  call $hydra.lib.maps.from_list
  local.get $g
  local.get $kv
  call $hydra.lib.pairs.first
  local.get $key_decoder
  local.get $k
  local.get $v
  local.get $g
  local.get $kv
  call $hydra.lib.pairs.second
  local.get $val_decoder
  call $hydra.lib.eithers.map
  call $hydra.lib.eithers.bind
  local.get $m
  call $hydra.lib.maps.to_list
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.decode_maybe (param $elem_decoder i32) (param $g i32) (param $term i32) (result i32)
  (local $opt i32)
  (local $stripped i32)
  (local $x i32)
  local.get $x
  local.get $x
  local.get $g
  local.get $term
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.bimap
  (block $end_term (result i32)
  (block $maybe
  local.get $stripped
  br_table $maybe $maybe
)
  local.get $g
  local.get $elem_decoder
  local.get $opt
  call $hydra.lib.eithers.map_maybe
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.decode_pair (param $first_decoder i32) (param $second_decoder i32) (param $g i32) (param $term i32) (result i32)
  (local $f i32)
  (local $p i32)
  (local $s i32)
  (local $stripped i32)
  (local $x i32)
  local.get $x
  local.get $x
  local.get $g
  local.get $term
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.bimap
  (block $end_term (result i32)
  (block $pair
  local.get $stripped
  br_table $pair $pair
)
  local.get $g
  local.get $p
  call $hydra.lib.pairs.first
  local.get $first_decoder
  local.get $f
  local.get $s
  local.get $g
  local.get $p
  call $hydra.lib.pairs.second
  local.get $second_decoder
  call $hydra.lib.eithers.map
  call $hydra.lib.eithers.bind
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.decode_set (param $elem_decoder i32) (param $g i32) (param $term i32) (result i32)
  (local $s i32)
  (local $stripped i32)
  (local $x i32)
  local.get $x
  local.get $x
  local.get $g
  local.get $term
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.bimap
  (block $end_term (result i32)
  (block $set
  local.get $stripped
  br_table $set $set
)
  call $hydra.lib.sets.from_list
  local.get $g
  local.get $elem_decoder
  local.get $s
  call $hydra.lib.sets.to_list
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.decode_unit (param $g i32) (param $term i32) (result i32)
  (local $stripped i32)
  (local $x i32)
  local.get $x
  local.get $x
  local.get $g
  local.get $term
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.bimap
  (block $end_term (result i32)
  (block $unit
  local.get $stripped
  br_table $unit $unit
)
  i32.const 1
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.decode_wrapped (param $body_decoder i32) (param $g i32) (param $term i32) (result i32)
  (local $stripped i32)
  (local $wt i32)
  (local $x i32)
  local.get $x
  local.get $x
  local.get $g
  local.get $term
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.bimap
  (block $end_term (result i32)
  (block $wrap
  local.get $stripped
  br_table $wrap $wrap
)
  local.get $g
  local.get $wt
  ;; project field: body
  local.get $body_decoder
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.either_term (param $cx i32) (param $left_fun i32) (param $right_fun i32) (param $graph i32) (param $term0 i32) (result i32)
  (local $et i32)
  (local $l i32)
  (local $r i32)
  (local $term i32)
  (local $x i32)
  local.get $cx
  local.get $graph
  local.get $term0
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $either
  local.get $term
  br_table $either $either
)
  i32.const 0
  local.get $x
  local.get $l
  local.get $left_fun
  call $hydra.lib.eithers.map
  i32.const 1
  local.get $x
  local.get $r
  local.get $right_fun
  call $hydra.lib.eithers.map
  local.get $et
  call $hydra.lib.eithers.either
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.either_type (param $cx i32) (param $typ i32) (result i32)
  (local $et i32)
  (local $stripped i32)
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $stripped
  (block $end_type (result i32)
  (block $either
  local.get $stripped
  br_table $either $either
)
  i32.const 1
  local.get $et
  br $end_type
)
)
  (func $hydra.extract.core.field (param $cx i32) (param $fname i32) (param $mapping i32) (param $graph i32) (param $fields i32) (result i32)
  (local $matching_fields i32)
  (local $stripped i32)
  nop
  nop
  call $hydra.lib.equality.equal
  local.get $fields
  call $hydra.lib.lists.filter
  local.set $matching_fields
  local.get $matching_fields
  call $hydra.lib.lists.null
  i32.const 0
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "field "
  nop
  call $hydra.lib.strings.cat2
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: " but found "
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: "no matching field"
  call $hydra.lib.strings.cat2
  local.get $cx
  local.get $matching_fields
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.equality.equal
  local.get $cx
  local.get $graph
  local.get $matching_fields
  call $hydra.lib.lists.head
  ;; project field: term
  call $hydra.lexical.strip_and_dereference_term
  local.get $stripped
  local.get $mapping
  call $hydra.lib.eithers.bind
  i32.const 0
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "single field"
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: " but found "
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: "multiple fields named "
  nop
  call $hydra.lib.strings.cat2
  call $hydra.lib.strings.cat2
  local.get $cx
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
)
  (func $hydra.extract.core.float32 (param $cx i32) (param $graph i32) (param $t i32) (result i32)
  (local $f i32)
  (local $l i32)
  local.get $cx
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $cx
  local.get $l
  call $hydra.extract.core.float_literal
  local.get $cx
  local.get $f
  call $hydra.extract.core.float32_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.float32_value (param $cx i32) (param $v i32) (result i32)
  (local $f i32)
  (block $end_float_value (result i32)
  (block $float32
  local.get $v
  br_table $float32 $float32
)
  i32.const 1
  local.get $f
  br $end_float_value
)
)
  (func $hydra.extract.core.float64 (param $cx i32) (param $graph i32) (param $t i32) (result i32)
  (local $f i32)
  (local $l i32)
  local.get $cx
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $cx
  local.get $l
  call $hydra.extract.core.float_literal
  local.get $cx
  local.get $f
  call $hydra.extract.core.float64_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.float64_value (param $cx i32) (param $v i32) (result i32)
  (local $f i32)
  (block $end_float_value (result i32)
  (block $float64
  local.get $v
  br_table $float64 $float64
)
  i32.const 1
  local.get $f
  br $end_float_value
)
)
  (func $hydra.extract.core.float_literal (param $cx i32) (param $lit i32) (result i32)
  (local $v i32)
  (block $end_literal (result i32)
  (block $float
  local.get $lit
  br_table $float $float
)
  i32.const 1
  local.get $v
  br $end_literal
)
)
  (func $hydra.extract.core.float_value (param $cx i32) (param $graph i32) (param $t i32) (result i32)
  (local $l i32)
  local.get $cx
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $cx
  local.get $l
  call $hydra.extract.core.float_literal
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.function_type (param $cx i32) (param $typ i32) (result i32)
  (local $ft i32)
  (local $stripped i32)
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $stripped
  (block $end_type (result i32)
  (block $function
  local.get $stripped
  br_table $function $function
)
  i32.const 1
  local.get $ft
  br $end_type
)
)
  (func $hydra.extract.core.injection (param $cx i32) (param $expected i32) (param $graph i32) (param $term0 i32) (result i32)
  (local $injection i32)
  (local $term i32)
  local.get $cx
  local.get $graph
  local.get $term0
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $union
  local.get $term
  br_table $union $union
)
  nop
  nop
  call $hydra.lib.equality.equal
  i32.const 1
  local.get $injection
  ;; project field: field
  i32.const 0
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "injection of type "
  nop
  call $hydra.lib.strings.cat2
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: " but found "
  call $hydra.lib.strings.cat2
  nop
  call $hydra.lib.strings.cat2
  local.get $cx
  call $hydra.lib.logic.if_else
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.int16 (param $cx i32) (param $graph i32) (param $t i32) (result i32)
  (local $i i32)
  (local $l i32)
  local.get $cx
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $cx
  local.get $l
  call $hydra.extract.core.integer_literal
  local.get $cx
  local.get $i
  call $hydra.extract.core.int16_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.int16_value (param $cx i32) (param $v i32) (result i32)
  (local $i i32)
  (block $end_integer_value (result i32)
  (block $int16
  local.get $v
  br_table $int16 $int16
)
  i32.const 1
  local.get $i
  br $end_integer_value
)
)
  (func $hydra.extract.core.int32 (param $cx i32) (param $graph i32) (param $t i32) (result i32)
  (local $i i32)
  (local $l i32)
  local.get $cx
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $cx
  local.get $l
  call $hydra.extract.core.integer_literal
  local.get $cx
  local.get $i
  call $hydra.extract.core.int32_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.int32_value (param $cx i32) (param $v i32) (result i32)
  (local $i i32)
  (block $end_integer_value (result i32)
  (block $int32
  local.get $v
  br_table $int32 $int32
)
  i32.const 1
  local.get $i
  br $end_integer_value
)
)
  (func $hydra.extract.core.int64 (param $cx i32) (param $graph i32) (param $t i32) (result i32)
  (local $i i32)
  (local $l i32)
  local.get $cx
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $cx
  local.get $l
  call $hydra.extract.core.integer_literal
  local.get $cx
  local.get $i
  call $hydra.extract.core.int64_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.int64_value (param $cx i32) (param $v i32) (result i32)
  (local $i i32)
  (block $end_integer_value (result i32)
  (block $int64
  local.get $v
  br_table $int64 $int64
)
  i32.const 1
  local.get $i
  br $end_integer_value
)
)
  (func $hydra.extract.core.int8 (param $cx i32) (param $graph i32) (param $t i32) (result i32)
  (local $i i32)
  (local $l i32)
  local.get $cx
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $cx
  local.get $l
  call $hydra.extract.core.integer_literal
  local.get $cx
  local.get $i
  call $hydra.extract.core.int8_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.int8_value (param $cx i32) (param $v i32) (result i32)
  (local $i i32)
  (block $end_integer_value (result i32)
  (block $int8
  local.get $v
  br_table $int8 $int8
)
  i32.const 1
  local.get $i
  br $end_integer_value
)
)
  (func $hydra.extract.core.integer_literal (param $cx i32) (param $lit i32) (result i32)
  (local $v i32)
  (block $end_literal (result i32)
  (block $integer
  local.get $lit
  br_table $integer $integer
)
  i32.const 1
  local.get $v
  br $end_literal
)
)
  (func $hydra.extract.core.integer_value (param $cx i32) (param $graph i32) (param $t i32) (result i32)
  (local $l i32)
  local.get $cx
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $cx
  local.get $l
  call $hydra.extract.core.integer_literal
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.lambda (param $cx i32) (param $graph i32) (param $term0 i32) (result i32)
  (local $function i32)
  (local $l i32)
  (local $term i32)
  local.get $cx
  local.get $graph
  local.get $term0
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $function
  local.get $term
  br_table $function $function
)
  (block $end_function (result i32)
  (block $lambda
  local.get $function
  br_table $lambda $lambda
)
  i32.const 1
  local.get $l
  br $end_function
)
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.lambda_body (param $cx i32) (param $graph i32) (param $term i32) (result i32)
  ;; project field: body
  local.get $cx
  local.get $graph
  local.get $term
  call $hydra.extract.core.lambda
  call $hydra.lib.eithers.map
)
  (func $hydra.extract.core.let (param $cx i32) (param $graph i32) (param $term0 i32) (result i32)
  (local $lt i32)
  (local $term i32)
  local.get $cx
  local.get $graph
  local.get $term0
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $let
  local.get $term
  br_table $let $let
)
  i32.const 1
  local.get $lt
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.let_binding (param $cx i32) (param $n i32) (param $graph i32) (param $term i32) (result i32)
  (local $let_expr i32)
  (local $matching_bindings i32)
  (local $name i32)
  local.get $n
  local.set $name
  local.get $cx
  local.get $graph
  local.get $term
  call $hydra.extract.core.let
  nop
  nop
  call $hydra.lib.equality.equal
  local.get $let_expr
  ;; project field: bindings
  call $hydra.lib.lists.filter
  local.set $matching_bindings
  local.get $matching_bindings
  call $hydra.lib.lists.null
  i32.const 0
  i32.const 0 ;; string: "no such binding: "
  local.get $n
  call $hydra.lib.strings.cat2
  local.get $cx
  local.get $matching_bindings
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.equality.equal
  i32.const 1
  local.get $matching_bindings
  call $hydra.lib.lists.head
  ;; project field: term
  i32.const 0
  i32.const 0 ;; string: "multiple bindings named "
  local.get $n
  call $hydra.lib.strings.cat2
  local.get $cx
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.list (param $cx i32) (param $graph i32) (param $term i32) (result i32)
  (local $l i32)
  (local $stripped i32)
  local.get $cx
  local.get $graph
  local.get $term
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $list
  local.get $stripped
  br_table $list $list
)
  i32.const 1
  local.get $l
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.list_head (param $cx i32) (param $graph i32) (param $term i32) (result i32)
  (local $l i32)
  local.get $cx
  local.get $graph
  local.get $term
  call $hydra.extract.core.list
  local.get $l
  call $hydra.lib.lists.null
  i32.const 0
  i32.const 0 ;; string: "empty list"
  local.get $cx
  i32.const 1
  local.get $l
  call $hydra.lib.lists.head
  call $hydra.lib.logic.if_else
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.list_of (param $cx i32) (param $f i32) (param $graph i32) (param $term i32) (result i32)
  (local $els i32)
  local.get $cx
  local.get $graph
  local.get $term
  call $hydra.extract.core.list
  local.get $f
  local.get $els
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.list_type (param $cx i32) (param $typ i32) (result i32)
  (local $stripped i32)
  (local $t i32)
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $stripped
  (block $end_type (result i32)
  (block $list
  local.get $stripped
  br_table $list $list
)
  i32.const 1
  local.get $t
  br $end_type
)
)
  (func $hydra.extract.core.literal (param $cx i32) (param $graph i32) (param $term0 i32) (result i32)
  (local $lit i32)
  (local $term i32)
  local.get $cx
  local.get $graph
  local.get $term0
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $literal
  local.get $term
  br_table $literal $literal
)
  i32.const 1
  local.get $lit
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.map (param $cx i32) (param $fk i32) (param $fv i32) (param $graph i32) (param $term0 i32) (result i32)
  (local $kterm i32)
  (local $kv_pair i32)
  (local $kval i32)
  (local $m i32)
  (local $pair i32)
  (local $term i32)
  (local $vterm i32)
  (local $vval i32)
  local.get $kv_pair
  call $hydra.lib.pairs.first
  local.set $kterm
  local.get $kv_pair
  call $hydra.lib.pairs.second
  local.set $vterm
  local.get $kterm
  local.get $fk
  local.get $vterm
  local.get $fv
  i32.const 1
  local.get $kval
  local.get $vval
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.set $pair
  local.get $cx
  local.get $graph
  local.get $term0
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $map
  local.get $term
  br_table $map $map
)
  call $hydra.lib.maps.from_list
  local.get $pair
  local.get $m
  call $hydra.lib.maps.to_list
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.map_type (param $cx i32) (param $typ i32) (result i32)
  (local $mt i32)
  (local $stripped i32)
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $stripped
  (block $end_type (result i32)
  (block $map
  local.get $stripped
  br_table $map $map
)
  i32.const 1
  local.get $mt
  br $end_type
)
)
  (func $hydra.extract.core.maybe_term (param $cx i32) (param $f i32) (param $graph i32) (param $term0 i32) (result i32)
  (local $mt i32)
  (local $t i32)
  (local $term i32)
  local.get $cx
  local.get $graph
  local.get $term0
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $maybe
  local.get $term
  br_table $maybe $maybe
)
  i32.const 1
  i32.const 0
  call $hydra.lib.maybes.pure
  local.get $t
  local.get $f
  call $hydra.lib.eithers.map
  local.get $mt
  call $hydra.lib.maybes.maybe
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.maybe_type (param $cx i32) (param $typ i32) (result i32)
  (local $stripped i32)
  (local $t i32)
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $stripped
  (block $end_type (result i32)
  (block $maybe
  local.get $stripped
  br_table $maybe $maybe
)
  i32.const 1
  local.get $t
  br $end_type
)
)
  (func $hydra.extract.core.n_args (param $cx i32) (param $name i32) (param $n i32) (param $args i32) (result i32)
  local.get $args
  call $hydra.lib.lists.length
  local.get $n
  call $hydra.lib.equality.equal
  i32.const 1
  i32.const 0
  i32.const 0 ;; string: "expected "
  i32.const 3
  ;; list elements follow
  local.get $n
  call $hydra.lib.literals.show_int32
  i32.const 0 ;; string: " arguments to primitive "
  nop
  call $hydra.lib.literals.show_string
  call $hydra.lib.strings.cat
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: " but found "
  call $hydra.lib.strings.cat2
  local.get $args
  call $hydra.lib.lists.length
  call $hydra.lib.literals.show_int32
  call $hydra.lib.strings.cat2
  local.get $cx
  call $hydra.lib.logic.if_else
)
  (func $hydra.extract.core.pair (param $cx i32) (param $kf i32) (param $vf i32) (param $graph i32) (param $term0 i32) (result i32)
  (local $k_val i32)
  (local $p i32)
  (local $term i32)
  (local $v_val i32)
  local.get $cx
  local.get $graph
  local.get $term0
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $pair
  local.get $term
  br_table $pair $pair
)
  local.get $p
  call $hydra.lib.pairs.first
  local.get $kf
  local.get $p
  call $hydra.lib.pairs.second
  local.get $vf
  i32.const 1
  local.get $k_val
  local.get $v_val
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.record (param $cx i32) (param $expected i32) (param $graph i32) (param $term0 i32) (result i32)
  (local $record i32)
  local.get $cx
  local.get $graph
  local.get $term0
  call $hydra.extract.core.term_record
  local.get $record
  ;; project field: type_name
  local.get $expected
  call $hydra.lib.equality.equal
  i32.const 1
  local.get $record
  ;; project field: fields
  i32.const 0
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "record of type "
  nop
  call $hydra.lib.strings.cat2
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: " but found "
  call $hydra.lib.strings.cat2
  nop
  call $hydra.lib.strings.cat2
  local.get $cx
  call $hydra.lib.logic.if_else
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.record_type (param $cx i32) (param $ename i32) (param $typ i32) (result i32)
  (local $fields i32)
  (local $stripped i32)
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $stripped
  (block $end_type (result i32)
  (block $record
  local.get $stripped
  br_table $record $record
)
  i32.const 1
  local.get $fields
  br $end_type
)
)
  (func $hydra.extract.core.require_field (param $field_name i32) (param $decoder i32) (param $field_map i32) (param $g i32) (result i32)
  (local $field_term i32)
  i32.const 0
  i32.const 3
  ;; list elements follow
  i32.const 0 ;; string: "missing field "
  local.get $field_name
  i32.const 0 ;; string: " in record"
  call $hydra.lib.strings.cat
  local.get $g
  local.get $field_term
  local.get $decoder
  local.get $field_name
  local.get $field_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
)
  (func $hydra.extract.core.set (param $cx i32) (param $graph i32) (param $term i32) (result i32)
  (local $s i32)
  (local $stripped i32)
  local.get $cx
  local.get $graph
  local.get $term
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $set
  local.get $stripped
  br_table $set $set
)
  i32.const 1
  local.get $s
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.set_of (param $cx i32) (param $f i32) (param $graph i32) (param $term i32) (result i32)
  (local $els i32)
  local.get $cx
  local.get $graph
  local.get $term
  call $hydra.extract.core.set
  local.get $f
  local.get $els
  call $hydra.lib.eithers.map_set
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.set_type (param $cx i32) (param $typ i32) (result i32)
  (local $stripped i32)
  (local $t i32)
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $stripped
  (block $end_type (result i32)
  (block $set
  local.get $stripped
  br_table $set $set
)
  i32.const 1
  local.get $t
  br $end_type
)
)
  (func $hydra.extract.core.string (param $cx i32) (param $graph i32) (param $t i32) (result i32)
  (local $l i32)
  local.get $cx
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $cx
  local.get $l
  call $hydra.extract.core.string_literal
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.string_literal (param $cx i32) (param $v i32) (result i32)
  (local $s i32)
  (block $end_literal (result i32)
  (block $string
  local.get $v
  br_table $string $string
)
  i32.const 1
  local.get $s
  br $end_literal
)
)
  (func $hydra.extract.core.term_record (param $cx i32) (param $graph i32) (param $term0 i32) (result i32)
  (local $record i32)
  (local $term i32)
  local.get $cx
  local.get $graph
  local.get $term0
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $record
  local.get $term
  br_table $record $record
)
  i32.const 1
  local.get $record
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.to_field_map (param $record i32) (result i32)
  (local $f i32)
  local.get $f
  ;; project field: name
  local.get $f
  ;; project field: term
  local.get $record
  ;; project field: fields
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
)
  (func $hydra.extract.core.uint16 (param $cx i32) (param $graph i32) (param $t i32) (result i32)
  (local $i i32)
  (local $l i32)
  local.get $cx
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $cx
  local.get $l
  call $hydra.extract.core.integer_literal
  local.get $cx
  local.get $i
  call $hydra.extract.core.uint16_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.uint16_value (param $cx i32) (param $v i32) (result i32)
  (local $i i32)
  (block $end_integer_value (result i32)
  (block $uint16
  local.get $v
  br_table $uint16 $uint16
)
  i32.const 1
  local.get $i
  br $end_integer_value
)
)
  (func $hydra.extract.core.uint32 (param $cx i32) (param $graph i32) (param $t i32) (result i32)
  (local $i i32)
  (local $l i32)
  local.get $cx
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $cx
  local.get $l
  call $hydra.extract.core.integer_literal
  local.get $cx
  local.get $i
  call $hydra.extract.core.uint32_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.uint32_value (param $cx i32) (param $v i32) (result i32)
  (local $i i32)
  (block $end_integer_value (result i32)
  (block $uint32
  local.get $v
  br_table $uint32 $uint32
)
  i32.const 1
  local.get $i
  br $end_integer_value
)
)
  (func $hydra.extract.core.uint64 (param $cx i32) (param $graph i32) (param $t i32) (result i32)
  (local $i i32)
  (local $l i32)
  local.get $cx
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $cx
  local.get $l
  call $hydra.extract.core.integer_literal
  local.get $cx
  local.get $i
  call $hydra.extract.core.uint64_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.uint64_value (param $cx i32) (param $v i32) (result i32)
  (local $i i32)
  (block $end_integer_value (result i32)
  (block $uint64
  local.get $v
  br_table $uint64 $uint64
)
  i32.const 1
  local.get $i
  br $end_integer_value
)
)
  (func $hydra.extract.core.uint8 (param $cx i32) (param $graph i32) (param $t i32) (result i32)
  (local $i i32)
  (local $l i32)
  local.get $cx
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $cx
  local.get $l
  call $hydra.extract.core.integer_literal
  local.get $cx
  local.get $i
  call $hydra.extract.core.uint8_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.uint8_value (param $cx i32) (param $v i32) (result i32)
  (local $i i32)
  (block $end_integer_value (result i32)
  (block $uint8
  local.get $v
  br_table $uint8 $uint8
)
  i32.const 1
  local.get $i
  br $end_integer_value
)
)
  (func $hydra.extract.core.union_type (param $cx i32) (param $ename i32) (param $typ i32) (result i32)
  (local $fields i32)
  (local $stripped i32)
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $stripped
  (block $end_type (result i32)
  (block $union
  local.get $stripped
  br_table $union $union
)
  i32.const 1
  local.get $fields
  br $end_type
)
)
  (func $hydra.extract.core.unit (param $cx i32) (param $term i32) (result i32)
  (block $end_term (result i32)
  (block $unit
  local.get $term
  br_table $unit $unit
)
  i32.const 1
  br $end_term
)
)
  (func $hydra.extract.core.unit_variant (param $cx i32) (param $tname i32) (param $graph i32) (param $term i32) (result i32)
  (local $field i32)
  local.get $cx
  local.get $tname
  local.get $graph
  local.get $term
  call $hydra.extract.core.injection
  local.get $cx
  local.get $field
  ;; project field: term
  call $hydra.extract.core.unit
  i32.const 1
  local.get $field
  ;; project field: name
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.wrap (param $cx i32) (param $expected i32) (param $graph i32) (param $term0 i32) (result i32)
  (local $term i32)
  (local $wrapped_term i32)
  local.get $cx
  local.get $graph
  local.get $term0
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $wrap
  local.get $term
  br_table $wrap $wrap
)
  nop
  nop
  call $hydra.lib.equality.equal
  i32.const 1
  local.get $wrapped_term
  ;; project field: body
  i32.const 0
  i32.const 0 ;; string: "expected "
  i32.const 0 ;; string: "wrapper of type "
  nop
  call $hydra.lib.strings.cat2
  call $hydra.lib.strings.cat2
  i32.const 0 ;; string: " but found "
  call $hydra.lib.strings.cat2
  nop
  call $hydra.lib.strings.cat2
  local.get $cx
  call $hydra.lib.logic.if_else
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.wrapped_type (param $cx i32) (param $ename i32) (param $typ i32) (result i32)
  (local $inner_type i32)
  (local $stripped i32)
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $stripped
  (block $end_type (result i32)
  (block $wrap
  local.get $stripped
  br_table $wrap $wrap
)
  i32.const 1
  local.get $inner_type
  br $end_type
)
)
)
