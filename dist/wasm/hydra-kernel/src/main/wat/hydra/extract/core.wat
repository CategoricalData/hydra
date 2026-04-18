(module
  (import "hydra.lexical" "hydra.lexical.strip_and_dereference_term" (func $hydra.lexical.strip_and_dereference_term (param i32) (param i32) (result i32) ) )
  (import "hydra.lexical" "hydra.lexical.strip_and_dereference_term_either" (func $hydra.lexical.strip_and_dereference_term_either (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bimap" (func $hydra.lib.eithers.bimap (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.bind" (func $hydra.lib.eithers.bind (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.either" (func $hydra.lib.eithers.either (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map" (func $hydra.lib.eithers.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_list" (func $hydra.lib.eithers.map_list (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_maybe" (func $hydra.lib.eithers.map_maybe (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.eithers" "hydra.lib.eithers.map_set" (func $hydra.lib.eithers.map_set (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.filter" (func $hydra.lib.lists.filter (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.find" (func $hydra.lib.lists.find (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.length" (func $hydra.lib.lists.length (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.maybe_head" (func $hydra.lib.lists.maybe_head (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.null" (func $hydra.lib.lists.null (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_int32" (func $hydra.lib.literals.show_int32 (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_string" (func $hydra.lib.literals.show_string (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.from_list" (func $hydra.lib.maps.from_list (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.lookup" (func $hydra.lib.maps.lookup (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.maybe" (func $hydra.lib.maybes.maybe (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.sets" "hydra.lib.sets.to_list" (func $hydra.lib.sets.to_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.show.core" "hydra.show.core.term" (func $hydra.show.core.term (param i32) (result i32) ) )
  (import "hydra.show.errors" "hydra.show.errors.error" (func $hydra.show.errors.error (param i32) (result i32) ) )
  (import "hydra.strip" "hydra.strip.deannotate_type" (func $hydra.strip.deannotate_type (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\18\00\00\00\20\61\72\67\75\6d\65\6e\74\73\20\74\6f\20\70\72\69\6d\69\74\69\76\65\20\0a\00\00\00\20\69\6e\20\72\65\63\6f\72\64\01\00\00\00\29\08\00\00\00\62\69\67\66\6c\6f\61\74\06\00\00\00\62\69\67\69\6e\74\06\00\00\00\62\69\6e\61\72\79\07\00\00\00\62\6f\6f\6c\65\61\6e\0e\00\00\00\63\61\73\65\20\73\74\61\74\65\6d\65\6e\74\18\00\00\00\63\61\73\65\20\73\74\61\74\65\6d\65\6e\74\20\66\6f\72\20\74\79\70\65\20\07\00\00\00\64\65\63\69\6d\61\6c\0b\00\00\00\65\69\74\68\65\72\20\74\79\70\65\0c\00\00\00\65\69\74\68\65\72\20\76\61\6c\75\65\0a\00\00\00\65\6d\70\74\79\20\6c\69\73\74\15\00\00\00\65\78\70\65\63\74\65\64\20\61\20\75\6e\69\74\20\76\61\6c\75\65\15\00\00\00\65\78\70\65\63\74\65\64\20\65\69\74\68\65\72\20\76\61\6c\75\65\0d\00\00\00\65\78\70\65\63\74\65\64\20\6c\69\73\74\0c\00\00\00\65\78\70\65\63\74\65\64\20\6d\61\70\17\00\00\00\65\78\70\65\63\74\65\64\20\6f\70\74\69\6f\6e\61\6c\20\76\61\6c\75\65\0d\00\00\00\65\78\70\65\63\74\65\64\20\70\61\69\72\0c\00\00\00\65\78\70\65\63\74\65\64\20\73\65\74\16\00\00\00\65\78\70\65\63\74\65\64\20\77\72\61\70\70\65\64\20\76\61\6c\75\65\06\00\00\00\66\69\65\6c\64\20\07\00\00\00\66\6c\6f\61\74\33\32\07\00\00\00\66\6c\6f\61\74\36\34\14\00\00\00\66\6c\6f\61\74\69\6e\67\2d\70\6f\69\6e\74\20\76\61\6c\75\65\0d\00\00\00\66\75\6e\63\74\69\6f\6e\20\74\79\70\65\09\00\00\00\69\6e\6a\65\63\74\69\6f\6e\12\00\00\00\69\6e\6a\65\63\74\69\6f\6e\20\6f\66\20\74\79\70\65\20\05\00\00\00\69\6e\74\31\36\05\00\00\00\69\6e\74\33\32\05\00\00\00\69\6e\74\36\34\04\00\00\00\69\6e\74\38\0d\00\00\00\69\6e\74\65\67\65\72\20\76\61\6c\75\65\06\00\00\00\6c\61\6d\62\64\61\08\00\00\00\6c\65\74\20\74\65\72\6d\04\00\00\00\6c\69\73\74\09\00\00\00\6c\69\73\74\20\74\79\70\65\07\00\00\00\6c\69\74\65\72\61\6c\03\00\00\00\6d\61\70\08\00\00\00\6d\61\70\20\74\79\70\65\0d\00\00\00\6d\61\74\63\68\69\6e\67\20\63\61\73\65\0a\00\00\00\6d\61\79\62\65\20\74\79\70\65\0b\00\00\00\6d\61\79\62\65\20\76\61\6c\75\65\0e\00\00\00\6d\69\73\73\69\6e\67\20\66\69\65\6c\64\20\16\00\00\00\6d\75\6c\74\69\70\6c\65\20\66\69\65\6c\64\73\20\6e\61\6d\65\64\20\10\00\00\00\6e\6f\20\6d\61\74\63\68\69\6e\67\20\63\61\73\65\11\00\00\00\6e\6f\20\6d\61\74\63\68\69\6e\67\20\66\69\65\6c\64\0e\00\00\00\6e\6f\6e\2d\65\6d\70\74\79\20\6c\69\73\74\04\00\00\00\70\61\69\72\06\00\00\00\72\65\63\6f\72\64\0f\00\00\00\72\65\63\6f\72\64\20\6f\66\20\74\79\70\65\20\0b\00\00\00\72\65\63\6f\72\64\20\74\79\70\65\03\00\00\00\73\65\74\08\00\00\00\73\65\74\20\74\79\70\65\0c\00\00\00\73\69\6e\67\6c\65\20\66\69\65\6c\64\06\00\00\00\73\74\72\69\6e\67\06\00\00\00\75\69\6e\74\31\36\06\00\00\00\75\69\6e\74\33\32\06\00\00\00\75\69\6e\74\36\34\05\00\00\00\75\69\6e\74\38\0a\00\00\00\75\6e\69\6f\6e\20\74\79\70\65\04\00\00\00\75\6e\69\74\05\00\00\00\77\72\61\70\28\0c\00\00\00\77\72\61\70\70\65\64\20\74\79\70\65\10\00\00\00\77\72\61\70\70\65\72\20\6f\66\20\74\79\70\65\20")
  (global $__bump_ptr (mut i32) i32.const 1984 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
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
  (export "hydra.extract.core.decimal" (func $hydra.extract.core.decimal) )
  (export "hydra.extract.core.decimal_literal" (func $hydra.extract.core.decimal_literal) )
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
  (export "hydra.extract.core.strip_with_decoding_error" (func $hydra.extract.core.strip_with_decoding_error) )
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
  (func $hydra.extract.core.bigfloat (param $graph i32) (param $t i32) (result i32)
  (local $f i32)
  (local $l i32)
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $l
  call $hydra.extract.core.float_literal
  local.get $f
  call $hydra.extract.core.bigfloat_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.bigfloat_value (param $v i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $f i32)
  (block $end_float_value (result i32)
  (block $bigfloat
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $bigfloat $bigfloat
)
  local.get $v
  drop
  i32.const 1
  local.get $f
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
)
  (func $hydra.extract.core.bigint (param $graph i32) (param $t i32) (result i32)
  (local $i i32)
  (local $l i32)
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $l
  call $hydra.extract.core.integer_literal
  local.get $i
  call $hydra.extract.core.bigint_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.bigint_value (param $v i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $i i32)
  (block $end_integer_value (result i32)
  (block $bigint
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $bigint $bigint
)
  local.get $v
  drop
  i32.const 1
  local.get $i
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
  (func $hydra.extract.core.binary (param $graph i32) (param $t i32) (result i32)
  (local $l i32)
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $l
  call $hydra.extract.core.binary_literal
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.binary_literal (param $v i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (block $end_literal (result i32)
  (block $binary
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $binary $binary
)
  local.get $v
  drop
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
  br $end_literal
)
)
  (func $hydra.extract.core.boolean (param $graph i32) (param $t i32) (result i32)
  (local $l i32)
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $l
  call $hydra.extract.core.boolean_literal
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.boolean_literal (param $v i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (block $end_literal (result i32)
  (block $boolean
  local.get $v
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
  br $end_literal
)
)
  (func $hydra.extract.core.case_field (param $name i32) (param $n i32) (param $graph i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $cs i32)
  (local $f i32)
  (local $field_name i32)
  (local $matching i32)
  (local $mf i32)
  local.get $n
  local.set $field_name
  local.get $name
  local.get $graph
  local.get $term
  call $hydra.extract.core.cases
  local.get $f
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.get $field_name
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.equality.equal
  local.get $cs
  i32.load offset=8
  call $hydra.lib.lists.find
  local.set $matching
  i32.const 0
  i32.const 4
  i32.const 6
  i32.const 1618
  i32.const 1708
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
  i32.const 1
  local.get $mf
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
  local.get $matching
  call $hydra.lib.maybes.maybe
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.cases (param $name i32) (param $graph i32) (param $term0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $cs i32)
  (local $term i32)
  (local $v i32)
  local.get $graph
  local.get $term0
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $cases
  local.get $term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $cases $cases
)
  local.get $v
  drop
  local.get $cs
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.get $name
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.equality.equal
  i32.const 1
  local.get $cs
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
  i32.const 4
  i32.const 6
  i32.const 1132
  local.get $name
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
  local.get $term
  call $hydra.show.core.term
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
  call $hydra.lib.logic.if_else
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.decimal (param $graph i32) (param $t i32) (result i32)
  (local $l i32)
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $l
  call $hydra.extract.core.decimal_literal
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.decimal_literal (param $v i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $d i32)
  (block $end_literal (result i32)
  (block $decimal
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $decimal $decimal
)
  local.get $v
  drop
  i32.const 1
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
  br $end_literal
)
)
  (func $hydra.extract.core.decode_either (param $left_decoder i32) (param $right_decoder i32) (param $g i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $e i32)
  (local $lv i32)
  (local $rv i32)
  (local $stripped i32)
  (local $v i32)
  (local $x i32)
  local.get $g
  local.get $term
  call $hydra.extract.core.strip_with_decoding_error
  (block $end_term (result i32)
  (block $either
  local.get $stripped
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
  i32.const 0
  local.get $x
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
  local.get $g
  drop
  local.get $lv
  drop
  local.get $left_decoder
  drop
  i32.const 0
  call $hydra.lib.eithers.map
  i32.const 1
  local.get $x
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
  local.get $g
  drop
  local.get $rv
  drop
  local.get $right_decoder
  drop
  i32.const 0
  call $hydra.lib.eithers.map
  local.get $e
  call $hydra.lib.eithers.either
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.decode_list (param $elem_decoder i32) (param $g i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $els i32)
  (local $stripped i32)
  (local $v i32)
  local.get $g
  local.get $term
  call $hydra.extract.core.strip_with_decoding_error
  (block $end_term (result i32)
  (block $list
  local.get $stripped
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
  local.get $g
  drop
  local.get $elem_decoder
  drop
  i32.const 0
  local.get $els
  call $hydra.lib.eithers.map_list
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.decode_map (param $key_decoder i32) (param $val_decoder i32) (param $g i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $k i32)
  (local $kv i32)
  (local $m i32)
  (local $stripped i32)
  (local $v i32)
  local.get $g
  local.get $term
  call $hydra.extract.core.strip_with_decoding_error
  (block $end_term (result i32)
  (block $map
  local.get $stripped
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
  i32.const 0
  local.get $g
  drop
  local.get $kv
  call $hydra.lib.pairs.first
  drop
  local.get $key_decoder
  drop
  i32.const 0
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
  local.get $g
  drop
  local.get $kv
  call $hydra.lib.pairs.second
  drop
  local.get $val_decoder
  drop
  i32.const 0
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
  (local $__rec_ptr i32)
  (local $opt i32)
  (local $stripped i32)
  (local $v i32)
  local.get $g
  local.get $term
  call $hydra.extract.core.strip_with_decoding_error
  (block $end_term (result i32)
  (block $maybe
  local.get $stripped
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
  local.get $g
  drop
  local.get $elem_decoder
  drop
  i32.const 0
  local.get $opt
  call $hydra.lib.eithers.map_maybe
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.decode_pair (param $first_decoder i32) (param $second_decoder i32) (param $g i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $f i32)
  (local $p i32)
  (local $s i32)
  (local $stripped i32)
  (local $v i32)
  local.get $g
  local.get $term
  call $hydra.extract.core.strip_with_decoding_error
  (block $end_term (result i32)
  (block $pair
  local.get $stripped
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
  local.get $g
  drop
  local.get $p
  call $hydra.lib.pairs.first
  drop
  local.get $first_decoder
  drop
  i32.const 0
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
  local.get $g
  drop
  local.get $p
  call $hydra.lib.pairs.second
  drop
  local.get $second_decoder
  drop
  i32.const 0
  call $hydra.lib.eithers.map
  call $hydra.lib.eithers.bind
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.decode_set (param $elem_decoder i32) (param $g i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $s i32)
  (local $stripped i32)
  (local $v i32)
  local.get $g
  local.get $term
  call $hydra.extract.core.strip_with_decoding_error
  (block $end_term (result i32)
  (block $set
  local.get $stripped
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
  i32.const 0
  local.get $g
  drop
  local.get $elem_decoder
  drop
  i32.const 0
  local.get $s
  call $hydra.lib.sets.to_list
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.decode_unit (param $g i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $stripped i32)
  (local $v i32)
  local.get $g
  local.get $term
  call $hydra.extract.core.strip_with_decoding_error
  (block $end_term (result i32)
  (block $unit
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $unit $unit
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
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.decode_wrapped (param $body_decoder i32) (param $g i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $stripped i32)
  (local $v i32)
  (local $wt i32)
  local.get $g
  local.get $term
  call $hydra.extract.core.strip_with_decoding_error
  (block $end_term (result i32)
  (block $wrap
  local.get $stripped
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
  local.get $g
  drop
  local.get $wt
  i32.load offset=4
  drop
  local.get $body_decoder
  drop
  i32.const 0
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.either_term (param $left_fun i32) (param $right_fun i32) (param $graph i32) (param $term0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $et i32)
  (local $l i32)
  (local $r i32)
  (local $term i32)
  (local $v i32)
  (local $x i32)
  local.get $graph
  local.get $term0
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $either
  local.get $term
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
  i32.const 0
  local.get $x
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
  local.get $l
  drop
  local.get $left_fun
  drop
  i32.const 0
  call $hydra.lib.eithers.map
  i32.const 1
  local.get $x
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
  local.get $r
  drop
  local.get $right_fun
  drop
  i32.const 0
  call $hydra.lib.eithers.map
  local.get $et
  call $hydra.lib.eithers.either
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.either_type (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $et i32)
  (local $stripped i32)
  (local $v i32)
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $stripped
  (block $end_type (result i32)
  (block $either
  local.get $stripped
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
  i32.const 1
  local.get $et
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
)
  (func $hydra.extract.core.field (param $fname i32) (param $mapping i32) (param $graph i32) (param $fields i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $f i32)
  (local $matching_fields i32)
  (local $mf i32)
  (local $no_match_err i32)
  (local $stripped i32)
  local.get $f
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.get $fname
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.equality.equal
  local.get $fields
  call $hydra.lib.lists.filter
  local.set $matching_fields
  i32.const 0
  i32.const 4
  i32.const 6
  i32.const 1385
  local.get $fname
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
  i32.const 1728
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
  local.set $no_match_err
  local.get $matching_fields
  call $hydra.lib.lists.null
  local.get $no_match_err
  local.get $matching_fields
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.equality.equal
  local.get $no_match_err
  local.get $graph
  local.get $mf
  i32.load offset=4
  call $hydra.lexical.strip_and_dereference_term
  local.get $stripped
  drop
  local.get $mapping
  drop
  i32.const 0
  call $hydra.lib.eithers.bind
  local.get $matching_fields
  call $hydra.lib.lists.maybe_head
  call $hydra.lib.maybes.maybe
  i32.const 0
  i32.const 4
  i32.const 6
  i32.const 1838
  i32.const 1682
  local.get $fname
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
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
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
)
  (func $hydra.extract.core.float32 (param $graph i32) (param $t i32) (result i32)
  (local $f i32)
  (local $l i32)
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $l
  call $hydra.extract.core.float_literal
  local.get $f
  call $hydra.extract.core.float32_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.float32_value (param $v i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $f i32)
  (block $end_float_value (result i32)
  (block $float32
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $float32 $float32
)
  local.get $v
  drop
  i32.const 1
  local.get $f
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
)
  (func $hydra.extract.core.float64 (param $graph i32) (param $t i32) (result i32)
  (local $f i32)
  (local $l i32)
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $l
  call $hydra.extract.core.float_literal
  local.get $f
  call $hydra.extract.core.float64_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.float64_value (param $v i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $f i32)
  (block $end_float_value (result i32)
  (block $float64
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $float64 $float64
)
  local.get $v
  drop
  i32.const 1
  local.get $f
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
)
  (func $hydra.extract.core.float_literal (param $lit i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $v i32)
  (block $end_literal (result i32)
  (block $float
  local.get $lit
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $float $float
)
  local.get $v
  drop
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
  br $end_literal
)
)
  (func $hydra.extract.core.float_value (param $graph i32) (param $t i32) (result i32)
  (local $l i32)
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $l
  call $hydra.extract.core.float_literal
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.function_type (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $ft i32)
  (local $stripped i32)
  (local $v i32)
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $stripped
  (block $end_type (result i32)
  (block $function
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $function $function
)
  local.get $v
  drop
  i32.const 1
  local.get $ft
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
)
  (func $hydra.extract.core.injection (param $expected i32) (param $graph i32) (param $term0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $injection i32)
  (local $term i32)
  (local $v i32)
  local.get $graph
  local.get $term0
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $inject
  local.get $term
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
  local.get $injection
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.get $expected
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.equality.equal
  i32.const 1
  local.get $injection
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
  i32.const 0
  i32.const 4
  i32.const 6
  i32.const 1471
  local.get $expected
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
  local.get $injection
  i32.load
  drop
  i32.const 0
  drop
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
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.int16 (param $graph i32) (param $t i32) (result i32)
  (local $i i32)
  (local $l i32)
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $l
  call $hydra.extract.core.integer_literal
  local.get $i
  call $hydra.extract.core.int16_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.int16_value (param $v i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $i i32)
  (block $end_integer_value (result i32)
  (block $int16
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $int16 $int16
)
  local.get $v
  drop
  i32.const 1
  local.get $i
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
  (func $hydra.extract.core.int32 (param $graph i32) (param $t i32) (result i32)
  (local $i i32)
  (local $l i32)
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $l
  call $hydra.extract.core.integer_literal
  local.get $i
  call $hydra.extract.core.int32_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.int32_value (param $v i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $i i32)
  (block $end_integer_value (result i32)
  (block $int32
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $int32 $int32
)
  local.get $v
  drop
  i32.const 1
  local.get $i
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
  (func $hydra.extract.core.int64 (param $graph i32) (param $t i32) (result i32)
  (local $i i32)
  (local $l i32)
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $l
  call $hydra.extract.core.integer_literal
  local.get $i
  call $hydra.extract.core.int64_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.int64_value (param $v i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $i i32)
  (block $end_integer_value (result i32)
  (block $int64
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $int64 $int64
)
  local.get $v
  drop
  i32.const 1
  local.get $i
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
  (func $hydra.extract.core.int8 (param $graph i32) (param $t i32) (result i32)
  (local $i i32)
  (local $l i32)
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $l
  call $hydra.extract.core.integer_literal
  local.get $i
  call $hydra.extract.core.int8_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.int8_value (param $v i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $i i32)
  (block $end_integer_value (result i32)
  (block $int8
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $int8 $int8
)
  local.get $v
  drop
  i32.const 1
  local.get $i
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
  (func $hydra.extract.core.integer_literal (param $lit i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $v i32)
  (block $end_literal (result i32)
  (block $integer
  local.get $lit
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $integer $integer
)
  local.get $v
  drop
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
  br $end_literal
)
)
  (func $hydra.extract.core.integer_value (param $graph i32) (param $t i32) (result i32)
  (local $l i32)
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $l
  call $hydra.extract.core.integer_literal
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.lambda (param $graph i32) (param $term0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $l i32)
  (local $term i32)
  (local $v i32)
  local.get $graph
  local.get $term0
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $lambda
  local.get $term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $lambda $lambda
)
  local.get $v
  drop
  i32.const 1
  local.get $l
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
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.lambda_body (param $graph i32) (param $term i32) (result i32)
  i32.const 0
  local.get $graph
  local.get $term
  call $hydra.extract.core.lambda
  call $hydra.lib.eithers.map
)
  (func $hydra.extract.core.let (param $graph i32) (param $term0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $lt i32)
  (local $term i32)
  (local $v i32)
  local.get $graph
  local.get $term0
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $let
  local.get $term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $let $let
)
  local.get $v
  drop
  i32.const 1
  local.get $lt
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
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.let_binding (param $n i32) (param $graph i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $b i32)
  (local $let_expr i32)
  (local $matching_bindings i32)
  (local $name i32)
  (local $no_binding_err i32)
  local.get $n
  local.set $name
  local.get $graph
  local.get $term
  call $hydra.extract.core.let
  local.get $b
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.get $name
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.equality.equal
  local.get $let_expr
  i32.load
  call $hydra.lib.lists.filter
  local.set $matching_bindings
  i32.const 0
  i32.const 4
  i32.const 4
  local.get $name
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
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
  local.set $no_binding_err
  local.get $matching_bindings
  call $hydra.lib.lists.null
  local.get $no_binding_err
  local.get $matching_bindings
  call $hydra.lib.lists.length
  i32.const 1
  call $hydra.lib.equality.equal
  local.get $no_binding_err
  i32.const 1
  local.get $b
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
  local.get $matching_bindings
  call $hydra.lib.lists.maybe_head
  call $hydra.lib.maybes.maybe
  i32.const 0
  i32.const 4
  i32.const 1
  local.get $name
  i32.const 4
  call $__alloc
  local.set $__rec_ptr
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
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.list (param $graph i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $l i32)
  (local $stripped i32)
  (local $v i32)
  local.get $graph
  local.get $term
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $list
  local.get $stripped
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
  i32.const 1
  local.get $l
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
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.list_head (param $graph i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $h i32)
  (local $l i32)
  local.get $graph
  local.get $term
  call $hydra.extract.core.list
  i32.const 0
  i32.const 4
  i32.const 6
  i32.const 1749
  i32.const 1202
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
  i32.const 1
  local.get $h
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
  local.get $l
  call $hydra.lib.lists.maybe_head
  call $hydra.lib.maybes.maybe
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.list_of (param $f i32) (param $graph i32) (param $term i32) (result i32)
  (local $els i32)
  local.get $graph
  local.get $term
  call $hydra.extract.core.list
  local.get $f
  local.get $els
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.list_type (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $stripped i32)
  (local $t i32)
  (local $v i32)
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $stripped
  (block $end_type (result i32)
  (block $list
  local.get $stripped
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
  i32.const 1
  local.get $t
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
)
  (func $hydra.extract.core.literal (param $graph i32) (param $term0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $lit i32)
  (local $term i32)
  (local $v i32)
  local.get $graph
  local.get $term0
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $literal
  local.get $term
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
  i32.const 1
  local.get $lit
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
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.map (param $fk i32) (param $fv i32) (param $graph i32) (param $term0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $kterm i32)
  (local $kv_pair i32)
  (local $kval i32)
  (local $m i32)
  (local $pair i32)
  (local $term i32)
  (local $v i32)
  (local $vterm i32)
  (local $vval i32)
  local.get $kv_pair
  call $hydra.lib.pairs.first
  local.set $kterm
  local.get $kv_pair
  call $hydra.lib.pairs.second
  local.set $vterm
  local.get $kterm
  drop
  local.get $fk
  drop
  i32.const 0
  local.get $vterm
  drop
  local.get $fv
  drop
  i32.const 0
  i32.const 1
  local.get $kval
  local.get $vval
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  local.set $pair
  local.get $graph
  local.get $term0
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $map
  local.get $term
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
  i32.const 0
  local.get $pair
  local.get $m
  call $hydra.lib.maps.to_list
  call $hydra.lib.eithers.map_list
  call $hydra.lib.eithers.map
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.map_type (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $mt i32)
  (local $stripped i32)
  (local $v i32)
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $stripped
  (block $end_type (result i32)
  (block $map
  local.get $stripped
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
  i32.const 1
  local.get $mt
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
)
  (func $hydra.extract.core.maybe_term (param $f i32) (param $graph i32) (param $term0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $mt i32)
  (local $t i32)
  (local $term i32)
  (local $v i32)
  local.get $graph
  local.get $term0
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $maybe
  local.get $term
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
  i32.const 0
  local.get $t
  drop
  local.get $f
  drop
  i32.const 0
  call $hydra.lib.eithers.map
  local.get $mt
  call $hydra.lib.maybes.maybe
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.maybe_type (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $stripped i32)
  (local $t i32)
  (local $v i32)
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $stripped
  (block $end_type (result i32)
  (block $maybe
  local.get $stripped
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
  local.get $t
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
)
  (func $hydra.extract.core.n_args (param $name i32) (param $n i32) (param $args i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  local.get $args
  call $hydra.lib.lists.length
  local.get $n
  call $hydra.lib.equality.equal
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
  i32.const 0
  i32.const 4
  i32.const 6
  local.get $n
  call $hydra.lib.literals.show_int32
  i32.const 1024
  local.get $name
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.literals.show_string
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 3
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=12
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
  local.get $args
  call $hydra.lib.lists.length
  call $hydra.lib.literals.show_int32
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
  call $hydra.lib.logic.if_else
)
  (func $hydra.extract.core.pair (param $kf i32) (param $vf i32) (param $graph i32) (param $term0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $k_val i32)
  (local $p i32)
  (local $term i32)
  (local $v i32)
  (local $v_val i32)
  local.get $graph
  local.get $term0
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $pair
  local.get $term
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
  drop
  local.get $kf
  drop
  i32.const 0
  local.get $p
  call $hydra.lib.pairs.second
  drop
  local.get $vf
  drop
  i32.const 0
  i32.const 1
  local.get $k_val
  local.get $v_val
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.record (param $expected i32) (param $graph i32) (param $term0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $record i32)
  local.get $graph
  local.get $term0
  call $hydra.extract.core.term_record
  local.get $record
  i32.load
  local.get $expected
  call $hydra.lib.equality.equal
  i32.const 1
  local.get $record
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
  i32.const 0
  i32.const 4
  i32.const 6
  i32.const 1785
  local.get $expected
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
  local.get $record
  i32.load
  drop
  i32.const 0
  drop
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
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.record_type (param $ename i32) (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $fields i32)
  (local $stripped i32)
  (local $v i32)
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $stripped
  (block $end_type (result i32)
  (block $record
  local.get $stripped
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
  i32.const 1
  local.get $fields
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
)
  (func $hydra.extract.core.require_field (param $field_name i32) (param $decoder i32) (param $field_map i32) (param $g i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $field_term i32)
  i32.const 0
  i32.const 1664
  local.get $field_name
  i32.const 1052
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.const 3
  i32.store
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store offset=12
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
  local.get $g
  drop
  local.get $field_term
  drop
  local.get $decoder
  drop
  i32.const 0
  local.get $field_name
  local.get $field_map
  call $hydra.lib.maps.lookup
  call $hydra.lib.maybes.maybe
)
  (func $hydra.extract.core.set (param $graph i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $s i32)
  (local $stripped i32)
  (local $v i32)
  local.get $graph
  local.get $term
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $set
  local.get $stripped
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
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.set_of (param $f i32) (param $graph i32) (param $term i32) (result i32)
  (local $els i32)
  local.get $graph
  local.get $term
  call $hydra.extract.core.set
  local.get $f
  local.get $els
  call $hydra.lib.eithers.map_set
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.set_type (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $stripped i32)
  (local $t i32)
  (local $v i32)
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $stripped
  (block $end_type (result i32)
  (block $set
  local.get $stripped
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
  i32.const 1
  local.get $t
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
)
  (func $hydra.extract.core.string (param $graph i32) (param $t i32) (result i32)
  (local $l i32)
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $l
  call $hydra.extract.core.string_literal
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.string_literal (param $v i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $s i32)
  (block $end_literal (result i32)
  (block $string
  local.get $v
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
  br $end_literal
)
)
  (func $hydra.extract.core.strip_with_decoding_error (param $g i32) (param $term i32) (result i32)
  (local $_e i32)
  (local $x i32)
  local.get $_e
  call $hydra.show.errors.error
  local.get $x
  local.get $g
  local.get $term
  call $hydra.lexical.strip_and_dereference_term_either
  call $hydra.lib.eithers.bimap
)
  (func $hydra.extract.core.term_record (param $graph i32) (param $term0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $record i32)
  (local $term i32)
  (local $v i32)
  local.get $graph
  local.get $term0
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $record
  local.get $term
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
  i32.const 1
  local.get $record
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
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.to_field_map (param $record i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $f i32)
  local.get $f
  i32.load
  local.get $f
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
  local.get $record
  i32.load offset=4
  call $hydra.lib.lists.map
  call $hydra.lib.maps.from_list
)
  (func $hydra.extract.core.uint16 (param $graph i32) (param $t i32) (result i32)
  (local $i i32)
  (local $l i32)
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $l
  call $hydra.extract.core.integer_literal
  local.get $i
  call $hydra.extract.core.uint16_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.uint16_value (param $v i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $i i32)
  (block $end_integer_value (result i32)
  (block $uint16
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $uint16 $uint16
)
  local.get $v
  drop
  i32.const 1
  local.get $i
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
  (func $hydra.extract.core.uint32 (param $graph i32) (param $t i32) (result i32)
  (local $i i32)
  (local $l i32)
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $l
  call $hydra.extract.core.integer_literal
  local.get $i
  call $hydra.extract.core.uint32_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.uint32_value (param $v i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $i i32)
  (block $end_integer_value (result i32)
  (block $uint32
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $uint32 $uint32
)
  local.get $v
  drop
  i32.const 1
  local.get $i
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
  (func $hydra.extract.core.uint64 (param $graph i32) (param $t i32) (result i32)
  (local $i i32)
  (local $l i32)
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $l
  call $hydra.extract.core.integer_literal
  local.get $i
  call $hydra.extract.core.uint64_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.uint64_value (param $v i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $i i32)
  (block $end_integer_value (result i32)
  (block $uint64
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $uint64 $uint64
)
  local.get $v
  drop
  i32.const 1
  local.get $i
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
  (func $hydra.extract.core.uint8 (param $graph i32) (param $t i32) (result i32)
  (local $i i32)
  (local $l i32)
  local.get $graph
  local.get $t
  call $hydra.extract.core.literal
  local.get $l
  call $hydra.extract.core.integer_literal
  local.get $i
  call $hydra.extract.core.uint8_value
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.uint8_value (param $v i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $i i32)
  (block $end_integer_value (result i32)
  (block $uint8
  local.get $v
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $uint8 $uint8
)
  local.get $v
  drop
  i32.const 1
  local.get $i
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
  (func $hydra.extract.core.union_type (param $ename i32) (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $fields i32)
  (local $stripped i32)
  (local $v i32)
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $stripped
  (block $end_type (result i32)
  (block $union
  local.get $stripped
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $union $union
)
  local.get $v
  drop
  i32.const 1
  local.get $fields
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
)
  (func $hydra.extract.core.unit (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $v i32)
  (block $end_term (result i32)
  (block $unit
  local.get $term
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $unit $unit
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
  br $end_term
)
)
  (func $hydra.extract.core.unit_variant (param $tname i32) (param $graph i32) (param $term i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $field i32)
  local.get $tname
  local.get $graph
  local.get $term
  call $hydra.extract.core.injection
  local.get $field
  i32.load offset=4
  call $hydra.extract.core.unit
  i32.const 1
  local.get $field
  i32.load
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
  call $hydra.lib.eithers.bind
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.wrap (param $expected i32) (param $graph i32) (param $term0 i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $term i32)
  (local $v i32)
  (local $wrapped_term i32)
  local.get $graph
  local.get $term0
  call $hydra.lexical.strip_and_dereference_term
  (block $end_term (result i32)
  (block $wrap
  local.get $term
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
  local.get $wrapped_term
  i32.load
  drop
  i32.const 0
  drop
  i32.const 0
  local.get $expected
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.equality.equal
  i32.const 1
  local.get $wrapped_term
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
  i32.const 0
  i32.const 4
  i32.const 6
  i32.const 1950
  local.get $expected
  drop
  i32.const 0
  drop
  i32.const 0
  call $hydra.lib.strings.cat2
  local.get $wrapped_term
  i32.load
  drop
  i32.const 0
  drop
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
  br $end_term
)
  call $hydra.lib.eithers.bind
)
  (func $hydra.extract.core.wrapped_type (param $ename i32) (param $typ i32) (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  (local $inner_type i32)
  (local $stripped i32)
  (local $v i32)
  local.get $typ
  call $hydra.strip.deannotate_type
  local.set $stripped
  (block $end_type (result i32)
  (block $wrap
  local.get $stripped
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
  i32.const 1
  local.get $inner_type
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
)
)
