(module
  (import "hydra.lib.equality" "hydra.lib.equality.equal" (func $hydra.lib.equality.equal (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.lt" (func $hydra.lib.equality.lt (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.equality" "hydra.lib.equality.lte" (func $hydra.lib.equality.lte (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.map" (func $hydra.lib.lists.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.lists" "hydra.lib.lists.pure" (func $hydra.lib.lists.pure (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.bigint_to_decimal" (func $hydra.lib.literals.bigint_to_decimal (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.decimal_to_bigint" (func $hydra.lib.literals.decimal_to_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_bigint" (func $hydra.lib.literals.show_bigint (param i32) (result i32) ) )
  (import "hydra.lib.literals" "hydra.lib.literals.show_decimal" (func $hydra.lib.literals.show_decimal (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.and" (func $hydra.lib.logic.and (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.logic" "hydra.lib.logic.if_else" (func $hydra.lib.logic.if_else (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maps" "hydra.lib.maps.to_list" (func $hydra.lib.maps.to_list (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.maybe_div" (func $hydra.lib.math.maybe_div (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.math" "hydra.lib.math.maybe_mod" (func $hydra.lib.math.maybe_mod (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.from_maybe" (func $hydra.lib.maybes.from_maybe (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.maybes" "hydra.lib.maybes.map" (func $hydra.lib.maybes.map (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.first" (func $hydra.lib.pairs.first (param i32) (result i32) ) )
  (import "hydra.lib.pairs" "hydra.lib.pairs.second" (func $hydra.lib.pairs.second (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat" (func $hydra.lib.strings.cat (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.cat2" (func $hydra.lib.strings.cat2 (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.from_list" (func $hydra.lib.strings.from_list (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.length" (func $hydra.lib.strings.length (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.maybe_char_at" (func $hydra.lib.strings.maybe_char_at (param i32) (param i32) (result i32) ) )
  (import "hydra.lib.strings" "hydra.lib.strings.to_list" (func $hydra.lib.strings.to_list (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.braces_list_adaptive" (func $hydra.serialization.braces_list_adaptive (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.bracket_list_adaptive" (func $hydra.serialization.bracket_list_adaptive (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.cst" (func $hydra.serialization.cst (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.ifx" (func $hydra.serialization.ifx (param i32) (param i32) (param i32) (result i32) ) )
  (import "hydra.serialization" "hydra.serialization.print_expr" (func $hydra.serialization.print_expr (param i32) (result i32) ) )
  (memory $memory 2 )
  (export "memory" (memory $memory) )
  (data (offset i32.const 1024 ) "\01\00\00\00\22\10\00\00\00\30\31\32\33\34\35\36\37\38\39\61\62\63\64\65\66\01\00\00\00\3a\01\00\00\00\3f\02\00\00\00\5c\22\02\00\00\00\5c\5c\02\00\00\00\5c\62\02\00\00\00\5c\66\02\00\00\00\5c\6e\02\00\00\00\5c\72\02\00\00\00\5c\74\04\00\00\00\5c\75\30\30\05\00\00\00\66\61\6c\73\65\04\00\00\00\6e\75\6c\6c\04\00\00\00\74\72\75\65")
  (global $__bump_ptr (mut i32) i32.const 1136 )
  (export "__bump_ptr" (global $__bump_ptr) )
  (func $__alloc (param $sz i32) (result i32)
  global.get $__bump_ptr
  global.get $__bump_ptr
  local.get $sz
  i32.add
  global.set $__bump_ptr
)
  (export "hydra.json.writer.colon_op" (func $hydra.json.writer.colon_op) )
  (export "hydra.json.writer.hex_byte" (func $hydra.json.writer.hex_byte) )
  (export "hydra.json.writer.json_string" (func $hydra.json.writer.json_string) )
  (export "hydra.json.writer.key_value_to_expr" (func $hydra.json.writer.key_value_to_expr) )
  (export "hydra.json.writer.print_json" (func $hydra.json.writer.print_json) )
  (export "hydra.json.writer.value_to_expr" (func $hydra.json.writer.value_to_expr) )
  (func $hydra.json.writer.colon_op (result i32)
  (local $__rec_ptr i32)
  (local $__rec_scratch i32)
  i32.const 1049
  i32.const 0
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
  i32.const 8
  call $__alloc
  local.set $__rec_ptr
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
  i32.const 0
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
  i32.const 16
  call $__alloc
  local.set $__rec_ptr
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
  local.set $__rec_scratch
  local.get $__rec_ptr
  local.get $__rec_scratch
  i32.store
  local.get $__rec_ptr
)
  (func $hydra.json.writer.hex_byte (param $c i32) (result i32)
  (local $ch i32)
  (local $hi i32)
  (local $i i32)
  (local $lo i32)
  (local $nibble i32)
  i32.const 1054
  local.get $ch
  call $hydra.lib.lists.pure
  call $hydra.lib.strings.from_list
  local.get $i
  i32.const 1029
  call $hydra.lib.strings.maybe_char_at
  call $hydra.lib.maybes.map
  call $hydra.lib.maybes.from_maybe
  local.set $nibble
  i32.const 0
  local.get $c
  i32.const 16
  call $hydra.lib.math.maybe_div
  call $hydra.lib.maybes.from_maybe
  drop
  local.get $nibble
  drop
  i32.const 0
  local.set $hi
  i32.const 0
  local.get $c
  i32.const 16
  call $hydra.lib.math.maybe_mod
  call $hydra.lib.maybes.from_maybe
  drop
  local.get $nibble
  drop
  i32.const 0
  local.set $lo
  local.get $hi
  local.get $lo
  call $hydra.lib.strings.cat2
)
  (func $hydra.json.writer.json_string (param $s i32) (result i32)
  (local $c i32)
  (local $escape i32)
  (local $escaped i32)
  (local $hex_escape i32)
  i32.const 1101
  local.get $c
  call $hydra.json.writer.hex_byte
  call $hydra.lib.strings.cat2
  local.set $hex_escape
  local.get $c
  i32.const 34
  call $hydra.lib.equality.equal
  i32.const 1059
  local.get $c
  i32.const 92
  call $hydra.lib.equality.equal
  i32.const 1065
  local.get $c
  i32.const 8
  call $hydra.lib.equality.equal
  i32.const 1071
  local.get $c
  i32.const 12
  call $hydra.lib.equality.equal
  i32.const 1077
  local.get $c
  i32.const 10
  call $hydra.lib.equality.equal
  i32.const 1083
  local.get $c
  i32.const 13
  call $hydra.lib.equality.equal
  i32.const 1089
  local.get $c
  i32.const 9
  call $hydra.lib.equality.equal
  i32.const 1095
  local.get $c
  i32.const 32
  call $hydra.lib.equality.lt
  local.get $c
  drop
  local.get $hex_escape
  drop
  i32.const 0
  local.get $c
  call $hydra.lib.lists.pure
  call $hydra.lib.strings.from_list
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  call $hydra.lib.logic.if_else
  local.set $escape
  local.get $escape
  local.get $s
  call $hydra.lib.strings.to_list
  call $hydra.lib.lists.map
  call $hydra.lib.strings.cat
  local.set $escaped
  i32.const 1024
  local.get $escaped
  call $hydra.lib.strings.cat2
  i32.const 1024
  call $hydra.lib.strings.cat2
)
  (func $hydra.json.writer.key_value_to_expr (param $pair i32) (result i32)
  (local $key i32)
  (local $value i32)
  local.get $pair
  call $hydra.lib.pairs.first
  local.set $key
  local.get $pair
  call $hydra.lib.pairs.second
  local.set $value
  i32.const 0
  local.get $key
  call $hydra.json.writer.json_string
  call $hydra.serialization.cst
  local.get $value
  call $hydra.json.writer.value_to_expr
  call $hydra.serialization.ifx
)
  (func $hydra.json.writer.print_json (param $value i32) (result i32)
  local.get $value
  call $hydra.json.writer.value_to_expr
  call $hydra.serialization.print_expr
)
  (func $hydra.json.writer.value_to_expr (param $value i32) (result i32)
  (local $__rec_ptr i32)
  (local $arr i32)
  (local $b i32)
  (local $is_whole i32)
  (local $n i32)
  (local $obj i32)
  (local $plain i32)
  (local $rounded i32)
  (local $s i32)
  (local $shown i32)
  (local $v i32)
  (block $end_value (result i32)
  (block $string
  (block $object
  (block $number
  (block $null
  (block $boolean
  (block $array
  local.get $value
  local.set $__rec_ptr
  local.get $__rec_ptr
  i32.load offset=4
  local.set $v
  local.get $__rec_ptr
  i32.load
  br_table $array $boolean $null $number $object $string $string
)
  local.get $v
  drop
  i32.const 0
  local.get $arr
  call $hydra.lib.lists.map
  call $hydra.serialization.bracket_list_adaptive
  br $end_value
)
  local.get $v
  drop
  local.get $b
  i32.const 1126
  i32.const 1109
  call $hydra.lib.logic.if_else
  call $hydra.serialization.cst
  br $end_value
)
  local.get $v
  drop
  i32.const 1118
  call $hydra.serialization.cst
  br $end_value
)
  local.get $v
  drop
  local.get $n
  call $hydra.lib.literals.decimal_to_bigint
  local.set $rounded
  local.get $n
  call $hydra.lib.literals.show_decimal
  local.set $shown
  local.get $n
  local.get $rounded
  call $hydra.lib.literals.bigint_to_decimal
  call $hydra.lib.equality.equal
  local.set $is_whole
  local.get $rounded
  call $hydra.lib.literals.show_bigint
  local.set $plain
  local.get $is_whole
  local.get $plain
  call $hydra.lib.strings.length
  local.get $shown
  call $hydra.lib.strings.length
  call $hydra.lib.equality.lte
  call $hydra.lib.logic.and
  local.get $plain
  local.get $shown
  call $hydra.lib.logic.if_else
  call $hydra.serialization.cst
  br $end_value
)
  local.get $v
  drop
  i32.const 0
  local.get $obj
  call $hydra.lib.maps.to_list
  call $hydra.lib.lists.map
  call $hydra.serialization.braces_list_adaptive
  br $end_value
)
  local.get $v
  drop
  local.get $s
  call $hydra.json.writer.json_string
  call $hydra.serialization.cst
  br $end_value
)
)
)
